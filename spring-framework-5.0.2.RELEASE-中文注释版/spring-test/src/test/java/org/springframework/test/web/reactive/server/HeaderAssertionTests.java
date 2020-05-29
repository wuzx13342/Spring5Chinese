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

package org.springframework.test.web.reactive.server;

import java.net.URI;
import java.util.concurrent.TimeUnit;

import org.junit.Test;
import reactor.core.publisher.MonoProcessor;

import org.springframework.http.CacheControl;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.http.client.reactive.MockClientHttpRequest;
import org.springframework.mock.http.client.reactive.MockClientHttpResponse;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for {@link HeaderAssertions}.
 *
 * @author Rossen Stoyanchev
 * @since 5.0
 */
public class HeaderAssertionTests {

	@Test
	public void valueEquals() {
		HttpHeaders headers = new HttpHeaders();
		headers.add("foo", "bar");
		HeaderAssertions assertions = headerAssertions(headers);

		// Success
		assertions.valueEquals("foo", "bar");

		try {
			assertions.valueEquals("what?!", "bar");
			fail("Missing header expected");
		}
		catch (AssertionError error) {
			// expected
		}

		try {
			assertions.valueEquals("foo", "what?!");
			fail("Wrong value expected");
		}
		catch (AssertionError error) {
			// expected
		}

		try {
			assertions.valueEquals("foo", "bar", "what?!");
			fail("Wrong # of values expected");
		}
		catch (AssertionError error) {
			// expected
		}
	}

	@Test
	public void valueEqualsWithMultipeValues() {
		HttpHeaders headers = new HttpHeaders();
		headers.add("foo", "bar");
		headers.add("foo", "baz");
		HeaderAssertions assertions = headerAssertions(headers);

		// Success
		assertions.valueEquals("foo", "bar", "baz");

		try {
			assertions.valueEquals("foo", "bar", "what?!");
			fail("Wrong value expected");
		}
		catch (AssertionError error) {
			// expected
		}

		try {
			assertions.valueEquals("foo", "bar");
			fail("Too few values expected");
		}
		catch (AssertionError error) {
			// expected
		}

	}

	@Test
	public void valueMatches() {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
		HeaderAssertions assertions = headerAssertions(headers);

		// Success
		assertions.valueMatches("Content-Type", ".*UTF-8.*");

		try {
			assertions.valueMatches("Content-Type", ".*ISO-8859-1.*");
			fail("Wrong pattern expected");
		}
		catch (AssertionError error) {
			Throwable cause = error.getCause();
			assertNotNull(cause);
			assertEquals("Response header 'Content-Type'=[application/json;charset=UTF-8] " +
					"does not match [.*ISO-8859-1.*]", cause.getMessage());
		}
	}

	@Test
	public void cacheControl() {
		CacheControl control = CacheControl.maxAge(1, TimeUnit.HOURS).noTransform();

		HttpHeaders headers = new HttpHeaders();
		headers.setCacheControl(control.getHeaderValue());
		HeaderAssertions assertions = headerAssertions(headers);

		// Success
		assertions.cacheControl(control);

		try {
			assertions.cacheControl(CacheControl.noStore());
			fail("Wrong value expected");
		}
		catch (AssertionError error) {
			// Expected
		}
	}


	private HeaderAssertions headerAssertions(HttpHeaders responseHeaders) {
		MockClientHttpRequest request = new MockClientHttpRequest(HttpMethod.GET, URI.create("/"));
		MockClientHttpResponse response = new MockClientHttpResponse(HttpStatus.OK);
		response.getHeaders().putAll(responseHeaders);

		MonoProcessor<byte[]> emptyContent = MonoProcessor.create();
		emptyContent.onComplete();

		ExchangeResult result = new ExchangeResult(request, response, emptyContent, emptyContent, null);
		return new HeaderAssertions(result, mock(WebTestClient.ResponseSpec.class));
	}

}
