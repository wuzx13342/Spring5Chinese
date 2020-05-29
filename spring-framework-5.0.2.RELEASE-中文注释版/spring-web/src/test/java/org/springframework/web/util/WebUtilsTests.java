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

package org.springframework.web.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import org.springframework.http.HttpHeaders;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServletServerHttpRequest;
import org.springframework.mock.web.test.MockHttpServletRequest;
import org.springframework.util.MultiValueMap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * @author Juergen Hoeller
 * @author Arjen Poutsma
 * @author Rossen Stoyanchev
 * @author Sebastien Deleuze
 */
public class WebUtilsTests {

	@Test
	public void findParameterValue() {
		Map<String, Object> params = new HashMap<>();
		params.put("myKey1", "myValue1");
		params.put("myKey2_myValue2", "xxx");
		params.put("myKey3_myValue3.x", "xxx");
		params.put("myKey4_myValue4.y", new String[] {"yyy"});

		assertNull(WebUtils.findParameterValue(params, "myKey0"));
		assertEquals("myValue1", WebUtils.findParameterValue(params, "myKey1"));
		assertEquals("myValue2", WebUtils.findParameterValue(params, "myKey2"));
		assertEquals("myValue3", WebUtils.findParameterValue(params, "myKey3"));
		assertEquals("myValue4", WebUtils.findParameterValue(params, "myKey4"));
	}

	@Test
	public void parseMatrixVariablesString() {
		MultiValueMap<String, String> variables;

		variables = WebUtils.parseMatrixVariables(null);
		assertEquals(0, variables.size());

		variables = WebUtils.parseMatrixVariables("year");
		assertEquals(1, variables.size());
		assertEquals("", variables.getFirst("year"));

		variables = WebUtils.parseMatrixVariables("year=2012");
		assertEquals(1, variables.size());
		assertEquals("2012", variables.getFirst("year"));

		variables = WebUtils.parseMatrixVariables("year=2012;colors=red,blue,green");
		assertEquals(2, variables.size());
		assertEquals(Arrays.asList("red", "blue", "green"), variables.get("colors"));
		assertEquals("2012", variables.getFirst("year"));

		variables = WebUtils.parseMatrixVariables(";year=2012;colors=red,blue,green;");
		assertEquals(2, variables.size());
		assertEquals(Arrays.asList("red", "blue", "green"), variables.get("colors"));
		assertEquals("2012", variables.getFirst("year"));

		variables = WebUtils.parseMatrixVariables("colors=red;colors=blue;colors=green");
		assertEquals(1, variables.size());
		assertEquals(Arrays.asList("red", "blue", "green"), variables.get("colors"));
	}

	@Test
	public void isValidOrigin() {
		List<String> allowed = Collections.emptyList();
		assertTrue(checkValidOrigin("mydomain1.com", -1, "http://mydomain1.com", allowed));
		assertFalse(checkValidOrigin("mydomain1.com", -1, "http://mydomain2.com", allowed));

		allowed = Collections.singletonList("*");
		assertTrue(checkValidOrigin("mydomain1.com", -1, "http://mydomain2.com", allowed));

		allowed = Collections.singletonList("http://mydomain1.com");
		assertTrue(checkValidOrigin("mydomain2.com", -1, "http://mydomain1.com", allowed));
		assertFalse(checkValidOrigin("mydomain2.com", -1, "http://mydomain3.com", allowed));
	}

	@Test
	public void isSameOrigin() {
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com"));
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com:80"));
		assertTrue(checkSameOrigin("mydomain1.com", 443, "https://mydomain1.com"));
		assertTrue(checkSameOrigin("mydomain1.com", 443, "https://mydomain1.com:443"));
		assertTrue(checkSameOrigin("mydomain1.com", 123, "http://mydomain1.com:123"));
		assertTrue(checkSameOrigin("mydomain1.com", -1, "ws://mydomain1.com"));
		assertTrue(checkSameOrigin("mydomain1.com", 443, "wss://mydomain1.com"));

		assertFalse(checkSameOrigin("mydomain1.com", -1, "http://mydomain2.com"));
		assertFalse(checkSameOrigin("mydomain1.com", -1, "https://mydomain1.com"));
		assertFalse(checkSameOrigin("mydomain1.com", -1, "invalid-origin"));

		// Handling of invalid origins as described in SPR-13478
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com/"));
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com:80/"));
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com/path"));
		assertTrue(checkSameOrigin("mydomain1.com", -1, "http://mydomain1.com:80/path"));
		assertFalse(checkSameOrigin("mydomain2.com", -1, "http://mydomain1.com/"));
		assertFalse(checkSameOrigin("mydomain2.com", -1, "http://mydomain1.com:80/"));
		assertFalse(checkSameOrigin("mydomain2.com", -1, "http://mydomain1.com/path"));
		assertFalse(checkSameOrigin("mydomain2.com", -1, "http://mydomain1.com:80/path"));

		// Handling of IPv6 hosts as described in SPR-13525
		assertTrue(checkSameOrigin("[::1]", -1, "http://[::1]"));
		assertTrue(checkSameOrigin("[::1]", 8080, "http://[::1]:8080"));
		assertTrue(checkSameOrigin(
				"[2001:0db8:0000:85a3:0000:0000:ac1f:8001]", -1,
				"http://[2001:0db8:0000:85a3:0000:0000:ac1f:8001]"));
		assertTrue(checkSameOrigin(
				"[2001:0db8:0000:85a3:0000:0000:ac1f:8001]", 8080,
				"http://[2001:0db8:0000:85a3:0000:0000:ac1f:8001]:8080"));
		assertFalse(checkSameOrigin("[::1]", -1, "http://[::1]:8080"));
		assertFalse(checkSameOrigin("[::1]", 8080,
				"http://[2001:0db8:0000:85a3:0000:0000:ac1f:8001]:8080"));
	}


	private boolean checkValidOrigin(String serverName, int port, String originHeader, List<String> allowed) {
		MockHttpServletRequest servletRequest = new MockHttpServletRequest();
		ServerHttpRequest request = new ServletServerHttpRequest(servletRequest);
		servletRequest.setServerName(serverName);
		if (port != -1) {
			servletRequest.setServerPort(port);
		}
		request.getHeaders().set(HttpHeaders.ORIGIN, originHeader);
		return WebUtils.isValidOrigin(request, allowed);
	}

	private boolean checkSameOrigin(String serverName, int port, String originHeader) {
		MockHttpServletRequest servletRequest = new MockHttpServletRequest();
		ServerHttpRequest request = new ServletServerHttpRequest(servletRequest);
		servletRequest.setServerName(serverName);
		if (port != -1) {
			servletRequest.setServerPort(port);
		}
		request.getHeaders().set(HttpHeaders.ORIGIN, originHeader);
		return WebUtils.isSameOrigin(request);
	}

}
