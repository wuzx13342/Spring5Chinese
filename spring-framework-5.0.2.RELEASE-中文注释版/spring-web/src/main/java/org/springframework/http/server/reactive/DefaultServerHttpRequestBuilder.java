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

package org.springframework.http.server.reactive;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import reactor.core.publisher.Flux;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

/**
 * Package-private default implementation of {@link ServerHttpRequest.Builder}.
 *
 * @author Rossen Stoyanchev
 * @author Sebastien Deleuze
 * @since 5.0
 */
class DefaultServerHttpRequestBuilder implements ServerHttpRequest.Builder {

	private URI uri;

	private HttpHeaders httpHeaders;

	private String httpMethodValue;

	private final MultiValueMap<String, HttpCookie> cookies;

	@Nullable
	private String uriPath;

	@Nullable
	private String contextPath;

	private Flux<DataBuffer> body;

	private final ServerHttpRequest originalRequest;


	public DefaultServerHttpRequestBuilder(ServerHttpRequest original) {
		Assert.notNull(original, "ServerHttpRequest is required");

		this.uri = original.getURI();
		this.httpMethodValue = original.getMethodValue();
		this.body = original.getBody();

		this.httpHeaders = new HttpHeaders();
		copyMultiValueMap(original.getHeaders(), this.httpHeaders);

		this.cookies = new LinkedMultiValueMap<>(original.getCookies().size());
		copyMultiValueMap(original.getCookies(), this.cookies);

		this.originalRequest = original;
	}

	private static <K, V> void copyMultiValueMap(MultiValueMap<K,V> source,
			MultiValueMap<K,V> destination) {

		for (Map.Entry<K, List<V>> entry : source.entrySet()) {
			K key = entry.getKey();
			List<V> values = new LinkedList<>(entry.getValue());
			destination.put(key, values);
		}
	}


	@Override
	public ServerHttpRequest.Builder method(HttpMethod httpMethod) {
		this.httpMethodValue = httpMethod.name();
		return this;
	}

	@Override
	public ServerHttpRequest.Builder uri(URI uri) {
		this.uri = uri;
		return this;
	}

	@Override
	public ServerHttpRequest.Builder path(String path) {
		this.uriPath = path;
		return this;
	}

	@Override
	public ServerHttpRequest.Builder contextPath(String contextPath) {
		this.contextPath = contextPath;
		return this;
	}

	@Override
	public ServerHttpRequest.Builder header(String key, String value) {
		this.httpHeaders.add(key, value);
		return this;
	}

	@Override
	public ServerHttpRequest.Builder headers(Consumer<HttpHeaders> headersConsumer) {
		Assert.notNull(headersConsumer, "'headersConsumer' must not be null");
		headersConsumer.accept(this.httpHeaders);
		return this;
	}

	@Override
	public ServerHttpRequest build() {
		URI uriToUse = getUriToUse();
		return new DefaultServerHttpRequest(uriToUse, this.contextPath, this.httpHeaders,
				this.httpMethodValue, this.cookies, this.body, this.originalRequest);

	}

	private URI getUriToUse() {
		if (this.uriPath == null) {
			return this.uri;
		}
		try {
			return new URI(this.uri.getScheme(), this.uri.getUserInfo(), uri.getHost(), uri.getPort(),
					uriPath, uri.getQuery(), uri.getFragment());
		}
		catch (URISyntaxException ex) {
			throw new IllegalStateException("Invalid URI path: \"" + this.uriPath + "\"");
		}
	}

	private static class DefaultServerHttpRequest extends AbstractServerHttpRequest {

		private final String methodValue;

		private final MultiValueMap<String, HttpCookie> cookies;

		@Nullable
		private final InetSocketAddress remoteAddress;

		@Nullable
		private final SslInfo sslInfo;

		private final Flux<DataBuffer> body;

		private final ServerHttpRequest originalRequest;


		public DefaultServerHttpRequest(URI uri, @Nullable String contextPath,
				HttpHeaders headers, String methodValue, MultiValueMap<String, HttpCookie> cookies,
				Flux<DataBuffer> body, ServerHttpRequest originalRequest) {

			super(uri, contextPath, headers);
			this.methodValue = methodValue;
			this.cookies = cookies;
			this.remoteAddress = originalRequest.getRemoteAddress();
			this.sslInfo = originalRequest.getSslInfo();
			this.body = body;
			this.originalRequest = originalRequest;
		}


		@Override
		public String getMethodValue() {
			return this.methodValue;
		}

		@Override
		protected MultiValueMap<String, HttpCookie> initCookies() {
			return this.cookies;
		}

		@Nullable
		@Override
		public InetSocketAddress getRemoteAddress() {
			return this.remoteAddress;
		}

		@Nullable
		@Override
		protected SslInfo initSslInfo() {
			return this.sslInfo;
		}

		@Override
		public Flux<DataBuffer> getBody() {
			return this.body;
		}

		@SuppressWarnings("unchecked")
		@Override
		public <T> T getNativeRequest() {
			return (T) this.originalRequest;
		}
	}

}
