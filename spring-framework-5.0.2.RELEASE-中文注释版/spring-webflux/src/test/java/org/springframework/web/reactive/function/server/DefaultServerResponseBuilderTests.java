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

package org.springframework.web.reactive.function.server;

import java.net.URI;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.junit.Test;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import org.springframework.http.CacheControl;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.mock.http.server.reactive.test.MockServerHttpResponse;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

/**
 * @author Arjen Poutsma
 */
public class DefaultServerResponseBuilderTests {

	@Test
	public void from() throws Exception {
		ServerResponse other = ServerResponse.ok().header("foo", "bar").build().block();
		Mono<ServerResponse> result = ServerResponse.from(other).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.OK.equals(response.statusCode()) &&
						"bar".equals(response.headers().getFirst("foo")))
				.expectComplete()
				.verify();
	}

	@Test
	public void status() throws Exception {
		Mono<ServerResponse> result = ServerResponse.status(HttpStatus.CREATED).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.CREATED.equals(response.statusCode()))
				.expectComplete()
				.verify();
	}

	@Test
	public void ok() throws Exception {
		Mono<ServerResponse> result = ServerResponse.ok().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.OK.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void created() throws Exception {
		URI location = URI.create("http://example.com");
		Mono<ServerResponse> result = ServerResponse.created(location).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.CREATED.equals(response.statusCode()) &&
						location.equals(response.headers().getLocation()))
				.expectComplete()
				.verify();
	}

	@Test
	public void accepted() throws Exception {
		Mono<ServerResponse> result = ServerResponse.accepted().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.ACCEPTED.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void noContent() throws Exception {
		Mono<ServerResponse> result = ServerResponse.noContent().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.NO_CONTENT.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void seeOther() throws Exception {
		URI location = URI.create("http://example.com");
		Mono<ServerResponse> result = ServerResponse.seeOther(location).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.SEE_OTHER.equals(response.statusCode()) &&
						location.equals(response.headers().getLocation()))
				.expectComplete()
				.verify();
	}

	@Test
	public void temporaryRedirect() throws Exception {
		URI location = URI.create("http://example.com");
		Mono<ServerResponse> result = ServerResponse.temporaryRedirect(location).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.TEMPORARY_REDIRECT.equals(response.statusCode()) &&
						location.equals(response.headers().getLocation()))
				.expectComplete()
				.verify();
	}

	@Test
	public void permanentRedirect() throws Exception {
		URI location = URI.create("http://example.com");
		Mono<ServerResponse> result = ServerResponse.permanentRedirect(location).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.PERMANENT_REDIRECT.equals(response.statusCode()) &&
						location.equals(response.headers().getLocation()))
				.expectComplete()
				.verify();
	}

	@Test
	public void badRequest() throws Exception {
		Mono<ServerResponse> result = ServerResponse.badRequest().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.BAD_REQUEST.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void notFound() throws Exception {
		Mono<ServerResponse> result = ServerResponse.notFound().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.NOT_FOUND.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void unprocessableEntity() throws Exception {
		Mono<ServerResponse> result = ServerResponse.unprocessableEntity().build();
		StepVerifier.create(result)
				.expectNextMatches(response -> HttpStatus.UNPROCESSABLE_ENTITY.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void allow() throws Exception {
		Mono<ServerResponse> result = ServerResponse.ok().allow(HttpMethod.GET).build();
		Set<HttpMethod> expected = EnumSet.of(HttpMethod.GET);
		StepVerifier.create(result)
				.expectNextMatches(response -> expected.equals(response.headers().getAllow()))
				.expectComplete()
				.verify();

	}

	@Test
	public void contentLength() throws Exception {
		Mono<ServerResponse> result = ServerResponse.ok().contentLength(42).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> Long.valueOf(42).equals(response.headers().getContentLength()))
				.expectComplete()
				.verify();

	}

	@Test
	public void contentType() throws Exception {
		Mono<ServerResponse>
				result = ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> MediaType.APPLICATION_JSON.equals(response.headers().getContentType()))
				.expectComplete()
				.verify();
	}

	@Test
	public void eTag() throws Exception {
		Mono<ServerResponse> result = ServerResponse.ok().eTag("foo").build();
		StepVerifier.create(result)
				.expectNextMatches(response -> "\"foo\"".equals(response.headers().getETag()))
				.expectComplete()
				.verify();

	}

	@Test
	public void lastModified() throws Exception {
		ZonedDateTime now = ZonedDateTime.now();
		Mono<ServerResponse> result = ServerResponse.ok().lastModified(now).build();
		Long expected = now.toInstant().toEpochMilli() / 1000;
		StepVerifier.create(result)
				.expectNextMatches(response -> expected.equals(response.headers().getLastModified() / 1000))
				.expectComplete()
				.verify();
	}

	@Test
	public void cacheControlTag() throws Exception {
		Mono<ServerResponse>
				result = ServerResponse.ok().cacheControl(CacheControl.noCache()).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> "no-cache".equals(response.headers().getCacheControl()))
				.expectComplete()
				.verify();
	}

	@Test
	public void varyBy() throws Exception {
		Mono<ServerResponse> result = ServerResponse.ok().varyBy("foo").build();
		List<String> expected = Collections.singletonList("foo");
		StepVerifier.create(result)
				.expectNextMatches(response -> expected.equals(response.headers().getVary()))
				.expectComplete()
				.verify();

	}

	@Test
	public void statusCode() throws Exception {
		HttpStatus statusCode = HttpStatus.ACCEPTED;
		Mono<ServerResponse> result = ServerResponse.status(statusCode).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> statusCode.equals(response.statusCode()))
				.expectComplete()
				.verify();

	}

	@Test
	public void headers() throws Exception {
		HttpHeaders newHeaders = new HttpHeaders();
		newHeaders.set("foo", "bar");
		Mono<ServerResponse> result =
				ServerResponse.ok().headers(headers -> headers.addAll(newHeaders)).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> newHeaders.equals(response.headers()))
				.expectComplete()
				.verify();

	}

	@Test
	public void cookies() throws Exception {
		MultiValueMap<String, ResponseCookie> newCookies = new LinkedMultiValueMap<>();
		newCookies.add("name", ResponseCookie.from("name", "value").build());
		Mono<ServerResponse> result =
				ServerResponse.ok().cookies(cookies -> cookies.addAll(newCookies)).build();
		StepVerifier.create(result)
				.expectNextMatches(response -> newCookies.equals(response.cookies()))
				.expectComplete()
				.verify();
	}

	@Test
	public void build() throws Exception {
		ResponseCookie cookie = ResponseCookie.from("name", "value").build();
		Mono<ServerResponse>
				result = ServerResponse.status(HttpStatus.CREATED)
				.header("MyKey", "MyValue")
				.cookie(cookie).build();

		ServerWebExchange exchange = mock(ServerWebExchange.class);
		MockServerHttpResponse response = new MockServerHttpResponse();
		when(exchange.getResponse()).thenReturn(response);
		ServerResponse.Context context = mock(ServerResponse.Context.class);

		result.flatMap(res -> res.writeTo(exchange, context)).block();

		assertEquals(HttpStatus.CREATED, response.getStatusCode());
		assertEquals("MyValue", response.getHeaders().getFirst("MyKey"));
		assertEquals("value", response.getCookies().getFirst("name").getValue());
		StepVerifier.create(response.getBody()).expectComplete().verify();
	}

	@Test
	public void buildVoidPublisher() throws Exception {
		Mono<Void> mono = Mono.empty();
		Mono<ServerResponse> result = ServerResponse.ok().build(mono);

		ServerWebExchange exchange = mock(ServerWebExchange.class);
		MockServerHttpResponse response = new MockServerHttpResponse();
		when(exchange.getResponse()).thenReturn(response);
		ServerResponse.Context context = mock(ServerResponse.Context.class);

		result.flatMap(res -> res.writeTo(exchange, context)).block();

		StepVerifier.create(response.getBody()).expectComplete().verify();
	}

	@Test(expected = IllegalArgumentException.class)
	public void bodyObjectPublisher() throws Exception {
		Mono<Void> mono = Mono.empty();

		ServerResponse.ok().syncBody(mono);
	}


}