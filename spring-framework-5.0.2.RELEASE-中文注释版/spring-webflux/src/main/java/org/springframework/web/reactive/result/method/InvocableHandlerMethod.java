/*
 * Copyright 2002-2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.web.reactive.result.method;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import reactor.core.publisher.Mono;

import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.MethodParameter;
import org.springframework.core.ParameterNameDiscoverer;
import org.springframework.core.ReactiveAdapter;
import org.springframework.core.ReactiveAdapterRegistry;
import org.springframework.http.HttpStatus;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.lang.Nullable;
import org.springframework.util.ClassUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.reactive.BindingContext;
import org.springframework.web.reactive.HandlerResult;
import org.springframework.web.server.ServerWebExchange;

/**
 * Extension of {@link HandlerMethod} that invokes the underlying method with
 * argument values resolved from the current HTTP request through a list of
 * {@link HandlerMethodArgumentResolver}.
 *
 * @author Rossen Stoyanchev
 * @author Juergen Hoeller
 * @since 5.0
 */
public class InvocableHandlerMethod extends HandlerMethod {

	private static final Mono<Object[]> EMPTY_ARGS = Mono.just(new Object[0]);

	private static final Object NO_ARG_VALUE = new Object();


	private List<HandlerMethodArgumentResolver> resolvers = new ArrayList<>();

	private ParameterNameDiscoverer parameterNameDiscoverer = new DefaultParameterNameDiscoverer();

	private ReactiveAdapterRegistry reactiveAdapterRegistry = ReactiveAdapterRegistry.getSharedInstance();


	public InvocableHandlerMethod(HandlerMethod handlerMethod) {
		super(handlerMethod);
	}

	public InvocableHandlerMethod(Object bean, Method method) {
		super(bean, method);
	}


	/**
	 * Configure the argument resolvers to use to use for resolving method
	 * argument values against a {@code ServerWebExchange}.
	 */
	public void setArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
		this.resolvers.clear();
		this.resolvers.addAll(resolvers);
	}

	/**
	 * Return the configured argument resolvers.
	 */
	public List<HandlerMethodArgumentResolver> getResolvers() {
		return this.resolvers;
	}

	/**
	 * Set the ParameterNameDiscoverer for resolving parameter names when needed
	 * (e.g. default request attribute name).
	 * <p>Default is a {@link DefaultParameterNameDiscoverer}.
	 */
	public void setParameterNameDiscoverer(ParameterNameDiscoverer nameDiscoverer) {
		this.parameterNameDiscoverer = nameDiscoverer;
	}

	/**
	 * Return the configured parameter name discoverer.
	 */
	public ParameterNameDiscoverer getParameterNameDiscoverer() {
		return this.parameterNameDiscoverer;
	}

	/**
	 * Configure a reactive registry. This is needed for cases where the response
	 * is fully handled within the controller in combination with an async void
	 * return value.
	 * <p>By default this is an instance of {@link ReactiveAdapterRegistry} with
	 * default settings.
	 * @param registry the registry to use
	 */
	public void setReactiveAdapterRegistry(ReactiveAdapterRegistry registry) {
		this.reactiveAdapterRegistry = registry;
	}


	/**
	 * Invoke the method for the given exchange.
	 * @param exchange the current exchange
	 * @param bindingContext the binding context to use
	 * @param providedArgs optional list of argument values to match by type
	 * @return Mono with a {@link HandlerResult}.
	 */
	public Mono<HandlerResult> invoke(ServerWebExchange exchange, BindingContext bindingContext,
			Object... providedArgs) {

		return resolveArguments(exchange, bindingContext, providedArgs).flatMap(args -> {
			try {
				Object value = doInvoke(args);

				HttpStatus status = getResponseStatus();
				if (status != null) {
					exchange.getResponse().setStatusCode(status);
				}

				MethodParameter returnType = getReturnType();
				ReactiveAdapter adapter = this.reactiveAdapterRegistry.getAdapter(returnType.getParameterType());
				boolean asyncVoid = isAsyncVoidReturnType(returnType, adapter);
				if ((value == null || asyncVoid) && isResponseHandled(args, exchange)) {
					logger.debug("Response fully handled in controller method");
					return asyncVoid ? Mono.from(adapter.toPublisher(value)) : Mono.empty();
				}

				HandlerResult result = new HandlerResult(this, value, returnType, bindingContext);
				return Mono.just(result);
			}
			catch (InvocationTargetException ex) {
				return Mono.error(ex.getTargetException());
			}
			catch (Throwable ex) {
				return Mono.error(new IllegalStateException(getInvocationErrorMessage(args)));
			}
		});
	}

	private Mono<Object[]> resolveArguments(ServerWebExchange exchange, BindingContext bindingContext,
			Object... providedArgs) {

		if (ObjectUtils.isEmpty(getMethodParameters())) {
			return EMPTY_ARGS;
		}
		try {
			List<Mono<Object>> argMonos = Stream.of(getMethodParameters())
					.map(param -> {
						param.initParameterNameDiscovery(this.parameterNameDiscoverer);
						return findProvidedArgument(param, providedArgs)
								.map(Mono::just)
								.orElseGet(() -> {
									HandlerMethodArgumentResolver resolver = findResolver(param);
									return resolveArg(resolver, param, bindingContext, exchange);
								});

					})
					.collect(Collectors.toList());

			// Create Mono with array of resolved values...
			return Mono.zip(argMonos, argValues ->
					Stream.of(argValues).map(o -> o != NO_ARG_VALUE ? o : null).toArray());
		}
		catch (Throwable ex) {
			return Mono.error(ex);
		}
	}

	private Optional<Object> findProvidedArgument(MethodParameter parameter, Object... providedArgs) {
		if (ObjectUtils.isEmpty(providedArgs)) {
			return Optional.empty();
		}
		return Arrays.stream(providedArgs)
				.filter(arg -> parameter.getParameterType().isInstance(arg))
				.findFirst();
	}

	private HandlerMethodArgumentResolver findResolver(MethodParameter param) {
		return this.resolvers.stream()
				.filter(r -> r.supportsParameter(param))
				.findFirst()
				.orElseThrow(() -> getArgumentError("No suitable resolver for", param, null));
	}

	private Mono<Object> resolveArg(HandlerMethodArgumentResolver resolver, MethodParameter parameter,
			BindingContext bindingContext, ServerWebExchange exchange) {

		try {
			return resolver.resolveArgument(parameter, bindingContext, exchange)
					.defaultIfEmpty(NO_ARG_VALUE)
					.doOnError(cause -> {
						if (logger.isDebugEnabled()) {
							logger.debug(getDetailedErrorMessage("Failed to resolve", parameter), cause);
						}
					});
		}
		catch (Exception ex) {
			throw getArgumentError("Failed to resolve", parameter, ex);
		}
	}

	private IllegalStateException getArgumentError(String text, MethodParameter parameter, @Nullable Throwable ex) {
		return new IllegalStateException(getDetailedErrorMessage(text, parameter), ex);
	}

	private String getDetailedErrorMessage(String text, MethodParameter param) {
		return text + " argument " + param.getParameterIndex() + " of type '" +
				param.getParameterType().getName() + "' on " + getBridgedMethod().toGenericString();
	}

	@Nullable
	private Object doInvoke(Object[] args) throws Exception {
		if (logger.isTraceEnabled()) {
			logger.trace("Invoking '" + ClassUtils.getQualifiedMethodName(getMethod(), getBeanType()) +
					"' with arguments " + Arrays.toString(args));
		}
		ReflectionUtils.makeAccessible(getBridgedMethod());
		Object returnValue = getBridgedMethod().invoke(getBean(), args);
		if (logger.isTraceEnabled()) {
			logger.trace("Method [" + ClassUtils.getQualifiedMethodName(getMethod(), getBeanType()) +
					"] returned [" + returnValue + "]");
		}
		return returnValue;
	}

	private String getInvocationErrorMessage(Object[] args) {
		String argumentDetails = IntStream.range(0, args.length)
				.mapToObj(i -> (args[i] != null ?
						"[" + i + "][type=" + args[i].getClass().getName() + "][value=" + args[i] + "]" :
						"[" + i + "][null]"))
				.collect(Collectors.joining(",", " ", " "));
		return "Failed to invoke handler method with resolved arguments:" + argumentDetails +
				"on " + getBridgedMethod().toGenericString();
	}

	private boolean isAsyncVoidReturnType(MethodParameter returnType,
			@Nullable ReactiveAdapter reactiveAdapter) {

		if (reactiveAdapter != null && reactiveAdapter.supportsEmpty()) {
			if (reactiveAdapter.isNoValue()) {
				return true;
			}
			Type parameterType = returnType.getGenericParameterType();
			if (parameterType instanceof ParameterizedType) {
				ParameterizedType type = (ParameterizedType) parameterType;
				if (type.getActualTypeArguments().length == 1) {
					return Void.class.equals(type.getActualTypeArguments()[0]);
				}
			}
		}
		return false;
	}

	private boolean isResponseHandled(Object[] args, ServerWebExchange exchange) {
		if (getResponseStatus() != null || exchange.isNotModified()) {
			return true;
		}
		for (Object arg : args) {
			if (arg instanceof ServerHttpResponse || arg instanceof ServerWebExchange) {
				return true;
			}
		}
		return false;
	}

}
