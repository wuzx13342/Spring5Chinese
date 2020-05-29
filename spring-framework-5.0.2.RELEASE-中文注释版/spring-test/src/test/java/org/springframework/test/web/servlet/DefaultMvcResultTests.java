/*
 * Copyright 2002-2014 the original author or authors.
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
package org.springframework.test.web.servlet;

import org.junit.Before;
import org.junit.Test;

import org.springframework.mock.web.MockHttpServletRequest;

import static org.junit.Assert.*;

/**
 * Test fixture for {@link DefaultMvcResult}.
 *
 * @author Rossen Stoyanchev
 */
public class DefaultMvcResultTests {

	private DefaultMvcResult mvcResult;

	@Before
	public void setup() {
		MockHttpServletRequest request = new MockHttpServletRequest();
		this.mvcResult = new DefaultMvcResult(request, null);
	}

	@Test
	public void getAsyncResultSuccess() throws Exception {
		this.mvcResult.setAsyncResult("Foo");
		assertEquals("Foo", this.mvcResult.getAsyncResult());
	}

	@Test(expected = IllegalStateException.class)
	public void getAsyncResultFailure() throws Exception {
		this.mvcResult.getAsyncResult(0);
	}

}
