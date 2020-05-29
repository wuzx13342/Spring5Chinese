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

package org.springframework.web.reactive.resource;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import reactor.core.Exceptions;
import reactor.core.publisher.Mono;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.ResolvableType;
import org.springframework.core.io.Resource;
import org.springframework.http.CacheControl;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.MediaTypeFactory;
import org.springframework.http.codec.ResourceHttpMessageWriter;
import org.springframework.http.server.PathContainer;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.reactive.HandlerMapping;
import org.springframework.web.server.MethodNotAllowedException;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebHandler;

/**
 * {@code HttpRequestHandler} that serves static resources in an optimized way
 * according to the guidelines of Page Speed, YSlow, etc.
 *
 * <p>The {@linkplain #setLocations "locations"} property takes a list of Spring
 * {@link Resource} locations from which static resources are allowed to
 * be served by this handler. Resources could be served from a classpath location,
 * e.g. "classpath:/META-INF/public-web-resources/", allowing convenient packaging
 * and serving of resources such as .js, .css, and others in jar files.
 *
 * <p>This request handler may also be configured with a
 * {@link #setResourceResolvers(List) resourcesResolver} and
 * {@link #setResourceTransformers(List) resourceTransformer} chains to support
 * arbitrary resolution and transformation of resources being served. By default a
 * {@link PathResourceResolver} simply finds resources based on the configured
 * "locations". An application can configure additional resolvers and
 * transformers such as the {@link VersionResourceResolver} which can resolve
 * and prepare URLs for resources with a version in the URL.
 *
 * <p>This handler also properly evaluates the {@code Last-Modified} header (if
 * present) so that a {@code 304} status code will be returned as appropriate,
 * avoiding unnecessary overhead for resources that are already cached by the
 * client.
 *
 * @author Rossen Stoyanchev
 * @author Brian Clozel
 * @since 5.0
 */
public class ResourceWebHandler implements WebHandler, InitializingBean {

	/** Set of supported HTTP methods */
	private static final Set<HttpMethod> SUPPORTED_METHODS = EnumSet.of(HttpMethod.GET, HttpMethod.HEAD);

	private static final ResponseStatusException NOT_FOUND_EXCEPTION =
			new ResponseStatusException(HttpStatus.NOT_FOUND);

	private static final Log logger = LogFactory.getLog(ResourceWebHandler.class);

	private final List<Resource> locations = new ArrayList<>(4);

	private final List<ResourceResolver> resourceResolvers = new ArrayList<>(4);

	private final List<ResourceTransformer> resourceTransformers = new ArrayList<>(4);

	@Nullable
	private CacheControl cacheControl;

	@Nullable
	private ResourceHttpMessageWriter resourceHttpMessageWriter;


	/**
	 * Set the {@code List} of {@code Resource} paths to use as sources
	 * for serving static resources.
	 */
	public void setLocations(@Nullable List<Resource> locations) {
		this.locations.clear();
		if (locations != null) {
			this.locations.addAll(locations);
		}
	}

	/**
	 * Return the {@code List} of {@code Resource} paths to use as sources
	 * for serving static resources.
	 */
	public List<Resource> getLocations() {
		return this.locations;
	}

	/**
	 * Configure the list of {@link ResourceResolver}s to use.
	 * <p>By default {@link PathResourceResolver} is configured. If using this property,
	 * it is recommended to add {@link PathResourceResolver} as the last resolver.
	 */
	public void setResourceResolvers(@Nullable List<ResourceResolver> resourceResolvers) {
		this.resourceResolvers.clear();
		if (resourceResolvers != null) {
			this.resourceResolvers.addAll(resourceResolvers);
		}
	}

	/**
	 * Return the list of configured resource resolvers.
	 */
	public List<ResourceResolver> getResourceResolvers() {
		return this.resourceResolvers;
	}

	/**
	 * Configure the list of {@link ResourceTransformer}s to use.
	 * <p>By default no transformers are configured for use.
	 */
	public void setResourceTransformers(@Nullable List<ResourceTransformer> resourceTransformers) {
		this.resourceTransformers.clear();
		if (resourceTransformers != null) {
			this.resourceTransformers.addAll(resourceTransformers);
		}
	}

	/**
	 * Return the list of configured resource transformers.
	 */
	public List<ResourceTransformer> getResourceTransformers() {
		return this.resourceTransformers;
	}

	/**
	 * Set the {@link org.springframework.http.CacheControl} instance to build
	 * the Cache-Control HTTP response header.
	 */
	public void setCacheControl(@Nullable CacheControl cacheControl) {
		this.cacheControl = cacheControl;
	}

	/**
	 * Return the {@link org.springframework.http.CacheControl} instance to build
	 * the Cache-Control HTTP response header.
	 */
	@Nullable
	public CacheControl getCacheControl() {
		return this.cacheControl;
	}

	/**
	 * Configure the {@link ResourceHttpMessageWriter} to use.
	 * <p>By default a {@link ResourceHttpMessageWriter} will be configured.
	 */
	public void setResourceHttpMessageWriter(@Nullable ResourceHttpMessageWriter httpMessageWriter) {
		this.resourceHttpMessageWriter = httpMessageWriter;
	}

	/**
	 * Return the configured resource message writer.
	 */
	@Nullable
	public ResourceHttpMessageWriter getResourceHttpMessageWriter() {
		return this.resourceHttpMessageWriter;
	}


	@Override
	public void afterPropertiesSet() throws Exception {
		if (this.resourceResolvers.isEmpty()) {
			this.resourceResolvers.add(new PathResourceResolver());
		}
		initAllowedLocations();
		if (getResourceHttpMessageWriter() == null) {
			this.resourceHttpMessageWriter = new ResourceHttpMessageWriter();
		}
	}

	/**
	 * Look for a {@code PathResourceResolver} among the configured resource
	 * resolvers and set its {@code allowedLocations} property (if empty) to
	 * match the {@link #setLocations locations} configured on this class.
	 */
	protected void initAllowedLocations() {
		if (CollectionUtils.isEmpty(this.locations)) {
			if (logger.isWarnEnabled()) {
				logger.warn("Locations list is empty. No resources will be served unless a " +
						"custom ResourceResolver is configured as an alternative to PathResourceResolver.");
			}
			return;
		}
		for (int i = getResourceResolvers().size() - 1; i >= 0; i--) {
			if (getResourceResolvers().get(i) instanceof PathResourceResolver) {
				PathResourceResolver resolver = (PathResourceResolver) getResourceResolvers().get(i);
				if (ObjectUtils.isEmpty(resolver.getAllowedLocations())) {
					resolver.setAllowedLocations(getLocations().toArray(new Resource[getLocations().size()]));
				}
				break;
			}
		}
	}


	/**
	 * Processes a resource request.
	 * <p>Checks for the existence of the requested resource in the configured list of locations.
	 * If the resource does not exist, a {@code 404} response will be returned to the client.
	 * If the resource exists, the request will be checked for the presence of the
	 * {@code Last-Modified} header, and its value will be compared against the last-modified
	 * timestamp of the given resource, returning a {@code 304} status code if the
	 * {@code Last-Modified} value  is greater. If the resource is newer than the
	 * {@code Last-Modified} value, or the header is not present, the content resource
	 * of the resource will be written to the response with caching headers
	 * set to expire one year in the future.
	 */
	@Override
	public Mono<Void> handle(ServerWebExchange exchange) {
		return getResource(exchange)
				.switchIfEmpty(Mono.defer(() -> {
					logger.trace("No matching resource found - returning 404");
					return Mono.error(NOT_FOUND_EXCEPTION);
				}))
				.flatMap(resource -> {
					try {
						if (HttpMethod.OPTIONS.matches(exchange.getRequest().getMethodValue())) {
							exchange.getResponse().getHeaders().add("Allow", "GET,HEAD,OPTIONS");
							return Mono.empty();
						}

						// Supported methods and required session
						HttpMethod httpMethod = exchange.getRequest().getMethod();
						if (!SUPPORTED_METHODS.contains(httpMethod)) {
							return Mono.error(new MethodNotAllowedException(
									exchange.getRequest().getMethodValue(), SUPPORTED_METHODS));
						}

						// Header phase
						if (exchange.checkNotModified(Instant.ofEpochMilli(resource.lastModified()))) {
							logger.trace("Resource not modified - returning 304");
							return Mono.empty();
						}

						// Apply cache settings, if any
						if (getCacheControl() != null) {
							String value = getCacheControl().getHeaderValue();
							if (value != null) {
								exchange.getResponse().getHeaders().setCacheControl(value);
							}
						}

						// Check the media type for the resource
						MediaType mediaType = MediaTypeFactory.getMediaType(resource).orElse(null);
						if (mediaType != null) {
							if (logger.isTraceEnabled()) {
								logger.trace("Determined media type '" + mediaType + "' for " + resource);
							}
						}
						else {
							if (logger.isTraceEnabled()) {
								logger.trace("No media type found " +
										"for " + resource + " - not sending a content-type header");
							}
						}

						// Content phase
						if (HttpMethod.HEAD.matches(exchange.getRequest().getMethodValue())) {
							setHeaders(exchange, resource, mediaType);
							exchange.getResponse().getHeaders().set(HttpHeaders.ACCEPT_RANGES, "bytes");
							logger.trace("HEAD request - skipping content");
							return Mono.empty();
						}

						setHeaders(exchange, resource, mediaType);
						ResourceHttpMessageWriter writer = getResourceHttpMessageWriter();
						Assert.state(writer != null, "No ResourceHttpMessageWriter");
						return writer.write(Mono.just(resource),
								null, ResolvableType.forClass(Resource.class), mediaType,
								exchange.getRequest(), exchange.getResponse(), Collections.emptyMap());
					}
					catch (IOException ex) {
						return Mono.error(ex);
					}
				});
	}

	protected Mono<Resource> getResource(ServerWebExchange exchange) {

		String name = HandlerMapping.PATH_WITHIN_HANDLER_MAPPING_ATTRIBUTE;
		PathContainer pathWithinHandler = exchange.getRequiredAttribute(name);
		String path = processPath(pathWithinHandler.value());
		if (!StringUtils.hasText(path) || isInvalidPath(path)) {
			if (logger.isTraceEnabled()) {
				logger.trace("Ignoring invalid resource path [" + path + "]");
			}
			return Mono.empty();
		}

		if (path.contains("%")) {
			try {
				// Use URLDecoder (vs UriUtils) to preserve potentially decoded UTF-8 chars
				if (isInvalidPath(URLDecoder.decode(path, "UTF-8"))) {
					if (logger.isTraceEnabled()) {
						logger.trace("Ignoring invalid resource path with escape sequences [" + path + "].");
					}
					return Mono.empty();
				}
			}
			catch (IllegalArgumentException ex) {
				// ignore
			}
			catch (UnsupportedEncodingException ex) {
				return Mono.error(Exceptions.propagate(ex));
			}
		}

		ResourceResolverChain resolveChain = createResolverChain();
		return resolveChain.resolveResource(exchange, path, getLocations())
				.flatMap(resource -> {
					ResourceTransformerChain transformerChain = createTransformerChain(resolveChain);
					return transformerChain.transform(exchange, resource);
				});
	}

	/**
	 * Process the given resource path to be used.
	 * <p>The default implementation replaces any combination of leading '/' and
	 * control characters (00-1F and 7F) with a single "/" or "". For example
	 * {@code "  // /// ////  foo/bar"} becomes {@code "/foo/bar"}.
	 */
	protected String processPath(String path) {
		boolean slash = false;
		for (int i = 0; i < path.length(); i++) {
			if (path.charAt(i) == '/') {
				slash = true;
			}
			else if (path.charAt(i) > ' ' && path.charAt(i) != 127) {
				if (i == 0 || (i == 1 && slash)) {
					return path;
				}
				path = slash ? "/" + path.substring(i) : path.substring(i);
				if (logger.isTraceEnabled()) {
					logger.trace("Path after trimming leading '/' and control characters: " + path);
				}
				return path;
			}
		}
		return (slash ? "/" : "");
	}

	/**
	 * Identifies invalid resource paths. By default rejects:
	 * <ul>
	 * <li>Paths that contain "WEB-INF" or "META-INF"
	 * <li>Paths that contain "../" after a call to
	 * {@link StringUtils#cleanPath}.
	 * <li>Paths that represent a {@link ResourceUtils#isUrl
	 * valid URL} or would represent one after the leading slash is removed.
	 * </ul>
	 * <p><strong>Note:</strong> this method assumes that leading, duplicate '/'
	 * or control characters (e.g. white space) have been trimmed so that the
	 * path starts predictably with a single '/' or does not have one.
	 * @param path the path to validate
	 * @return {@code true} if the path is invalid, {@code false} otherwise
	 */
	protected boolean isInvalidPath(String path) {
		if (logger.isTraceEnabled()) {
			logger.trace("Applying \"invalid path\" checks to path: " + path);
		}
		if (path.contains("WEB-INF") || path.contains("META-INF")) {
			if (logger.isTraceEnabled()) {
				logger.trace("Path contains \"WEB-INF\" or \"META-INF\".");
			}
			return true;
		}
		if (path.contains(":/")) {
			String relativePath = (path.charAt(0) == '/' ? path.substring(1) : path);
			if (ResourceUtils.isUrl(relativePath) || relativePath.startsWith("url:")) {
				if (logger.isTraceEnabled()) {
					logger.trace("Path represents URL or has \"url:\" prefix.");
				}
				return true;
			}
		}
		if (path.contains("..")) {
			path = StringUtils.cleanPath(path);
			if (path.contains("../")) {
				if (logger.isTraceEnabled()) {
					logger.trace("Path contains \"../\" after call to StringUtils#cleanPath.");
				}
				return true;
			}
		}
		return false;
	}

	private ResourceResolverChain createResolverChain() {
		return new DefaultResourceResolverChain(getResourceResolvers());
	}

	private ResourceTransformerChain createTransformerChain(ResourceResolverChain resolverChain) {
		return new DefaultResourceTransformerChain(resolverChain, getResourceTransformers());
	}

	/**
	 * Set headers on the response. Called for both GET and HEAD requests.
	 * @param exchange current exchange
	 * @param resource the identified resource (never {@code null})
	 * @param mediaType the resource's media type (never {@code null})
	 */
	protected void setHeaders(ServerWebExchange exchange, Resource resource, @Nullable MediaType mediaType)
			throws IOException {

		HttpHeaders headers = exchange.getResponse().getHeaders();

		long length = resource.contentLength();
		headers.setContentLength(length);

		if (mediaType != null) {
			headers.setContentType(mediaType);
		}
		if (resource instanceof HttpResource) {
			HttpHeaders resourceHeaders = ((HttpResource) resource).getResponseHeaders();
			exchange.getResponse().getHeaders().putAll(resourceHeaders);
		}
	}


	@Override
	public String toString() {
		return "ResourceWebHandler [locations=" + getLocations() + ", resolvers=" + getResourceResolvers() + "]";
	}

}
