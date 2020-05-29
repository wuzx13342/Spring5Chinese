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

import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import reactor.core.publisher.Mono;

import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.mock.http.server.reactive.test.MockServerHttpRequest;
import org.springframework.mock.web.test.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import static org.junit.Assert.assertEquals;

/**
 * Unit tests for {@code ResourceTransformerSupport}.
 *
 * @author Rossen Stoyanchev
 * @author Brian Clozel
 */
public class ResourceTransformerSupportTests {

	private ResourceTransformerChain transformerChain;

	private TestResourceTransformerSupport transformer;


	@Before
	public void setup() {
		VersionResourceResolver versionResolver = new VersionResourceResolver();
		versionResolver.setStrategyMap(Collections.singletonMap("/**", new ContentVersionStrategy()));
		PathResourceResolver pathResolver = new PathResourceResolver();
		pathResolver.setAllowedLocations(new ClassPathResource("test/", getClass()));
		List<ResourceResolver> resolvers = Arrays.asList(versionResolver, pathResolver);
		this.transformerChain = new DefaultResourceTransformerChain(new DefaultResourceResolverChain(resolvers), null);

		this.transformer = new TestResourceTransformerSupport();
		this.transformer.setResourceUrlProvider(createResourceUrlProvider(resolvers));
	}

	private ResourceUrlProvider createResourceUrlProvider(List<ResourceResolver> resolvers) {
		ResourceWebHandler handler = new ResourceWebHandler();
		handler.setLocations(Collections.singletonList(new ClassPathResource("test/", getClass())));
		handler.setResourceResolvers(resolvers);
		ResourceUrlProvider urlProvider = new ResourceUrlProvider();
		urlProvider.registerHandlers(Collections.singletonMap("/resources/**", handler));
		return urlProvider;
	}


	@Test
	public void resolveUrlPath() throws Exception {
		MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/resources/main.css"));
		String resourcePath = "/resources/bar.css";
		Resource css = new ClassPathResource("test/main.css", getClass());
		String actual = this.transformer.resolveUrlPath(
				resourcePath, exchange, css, this.transformerChain).block(Duration.ofSeconds(5));

		assertEquals("/resources/bar-11e16cf79faee7ac698c805cf28248d2.css", actual);
		assertEquals("/resources/bar-11e16cf79faee7ac698c805cf28248d2.css", actual);
	}

	@Test
	public void resolveUrlPathWithRelativePath() throws Exception {
		Resource css = new ClassPathResource("test/main.css", getClass());
		MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(""));
		String actual = this.transformer.resolveUrlPath(
				"bar.css", exchange, css, this.transformerChain).block(Duration.ofSeconds(5));

		assertEquals("bar-11e16cf79faee7ac698c805cf28248d2.css", actual);
	}

	@Test
	public void resolveUrlPathWithRelativePathInParentDirectory() throws Exception {
		Resource imagePng = new ClassPathResource("test/images/image.png", getClass());
		MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(""));
		String actual = this.transformer.resolveUrlPath(
				"../bar.css", exchange, imagePng, this.transformerChain).block(Duration.ofSeconds(5));

		assertEquals("../bar-11e16cf79faee7ac698c805cf28248d2.css", actual);
	}


	private static class TestResourceTransformerSupport extends ResourceTransformerSupport {

		@Override
		public Mono<Resource> transform(ServerWebExchange exchange, Resource resource,
				ResourceTransformerChain chain) {

			return Mono.error(new IllegalStateException("Should never be called"));
		}
	}

}
