/*
 * Copyright 2002-2016 the original author or authors.
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

package org.springframework.http.server.reactive;

import java.net.URISyntaxException;
import java.util.function.BiFunction;

import io.netty.handler.codec.http.HttpResponseStatus;
import reactor.core.publisher.Mono;
import reactor.ipc.netty.http.server.HttpServerRequest;
import reactor.ipc.netty.http.server.HttpServerResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.HttpMethod;
import org.springframework.util.Assert;

/**
 * Adapt {@link HttpHandler} to the Reactor Netty channel handling function.
 *
 * @author Stephane Maldini
 * @since 5.0
 */
public class ReactorHttpHandlerAdapter
		implements BiFunction<HttpServerRequest, HttpServerResponse, Mono<Void>> {

	private static final Log logger = LogFactory.getLog(ReactorHttpHandlerAdapter.class);


	private final HttpHandler httpHandler;


	public ReactorHttpHandlerAdapter(HttpHandler httpHandler) {
		Assert.notNull(httpHandler, "HttpHandler must not be null");
		this.httpHandler = httpHandler;
	}


	@Override
	public Mono<Void> apply(HttpServerRequest request, HttpServerResponse response) {

		NettyDataBufferFactory bufferFactory = new NettyDataBufferFactory(response.alloc());
		ServerHttpRequest adaptedRequest;
		ServerHttpResponse adaptedResponse;
		try {
			adaptedRequest = new ReactorServerHttpRequest(request, bufferFactory);
			adaptedResponse = new ReactorServerHttpResponse(response, bufferFactory);
		}
		catch (URISyntaxException ex) {
			logger.error("Invalid URL " + ex.getMessage(), ex);
			response.status(HttpResponseStatus.BAD_REQUEST);
			return Mono.empty();
		}

		if (HttpMethod.HEAD.equals(adaptedRequest.getMethod())) {
			adaptedResponse = new HttpHeadResponseDecorator(adaptedResponse);
		}

		return this.httpHandler.handle(adaptedRequest, adaptedResponse)
				.onErrorResume(ex -> {
					logger.error("Could not complete request", ex);
					response.status(HttpResponseStatus.INTERNAL_SERVER_ERROR);
					return Mono.empty();
				})
				.doOnSuccess(aVoid -> logger.debug("Successfully completed request"));
	}

}
