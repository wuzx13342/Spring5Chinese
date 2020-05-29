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

package org.springframework.web.reactive;

import java.nio.charset.StandardCharsets;
import java.time.Duration;

import org.junit.Before;
import org.junit.Test;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.http.server.reactive.AbstractHttpHandlerIntegrationTests;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.client.WebClient;

import static org.junit.Assert.assertTrue;

/**
 * @author Sebastien Deleuze
 * @since 5.0
 */
public class FlushingIntegrationTests extends AbstractHttpHandlerIntegrationTests {

	private WebClient webClient;


	@Before
	public void setup() throws Exception {
		super.setup();
		this.webClient = WebClient.create("http://localhost:" + this.port);
	}


	@Test
	public void writeAndFlushWith() throws Exception {
		Mono<String> result = this.webClient.get()
				.uri("/write-and-flush")
				.retrieve()
				.bodyToFlux(String.class)
				.takeUntil(s -> s.endsWith("data1"))
				.reduce((s1, s2) -> s1 + s2);

		StepVerifier.create(result)
				.expectNext("data0data1")
				.expectComplete()
				.verify(Duration.ofSeconds(5L));
	}

	@Test  // SPR-14991
	public void writeAndAutoFlushOnComplete() {
		Mono<String> result = this.webClient.get()
				.uri("/write-and-complete")
				.retrieve()
				.bodyToFlux(String.class)
				.reduce((s1, s2) -> s1 + s2);

		try {
			StepVerifier.create(result)
					.consumeNextWith(value -> assertTrue(value.length() == 20000 * "0123456789".length()))
					.expectComplete()
					.verify(Duration.ofSeconds(5L));
		}
		catch (AssertionError err) {
			String os = System.getProperty("os.name").toLowerCase();
			if (os.contains("windows") && err.getMessage().startsWith("VerifySubscriber timed out")) {
				// TODO: Reactor usually times out on Windows ...
				err.printStackTrace();
				return;
			}
			throw err;
		}
	}

	@Test  // SPR-14992
	public void writeAndAutoFlushBeforeComplete() {
		Mono<String> result = this.webClient.get()
				.uri("/write-and-never-complete")
				.retrieve()
				.bodyToFlux(String.class)
				.next();

		StepVerifier.create(result)
				.expectNextMatches(s -> s.startsWith("0123456789"))
				.expectComplete()
				.verify(Duration.ofSeconds(5L));
	}


	@Override
	protected HttpHandler createHttpHandler() {
		return new FlushingHandler();
	}


	private static class FlushingHandler implements HttpHandler {

		@Override
		public Mono<Void> handle(ServerHttpRequest request, ServerHttpResponse response) {
			String path = request.getURI().getPath();
			if (path.endsWith("write-and-flush")) {
				Flux<Publisher<DataBuffer>> responseBody = Flux
						.interval(Duration.ofMillis(50))
						.map(l -> toDataBuffer("data" + l, response.bufferFactory()))
						.take(2)
						.map(Flux::just);
				responseBody = responseBody.concatWith(Flux.never());
				return response.writeAndFlushWith(responseBody);
			}
			else if (path.endsWith("write-and-complete")) {
				Flux<DataBuffer> responseBody = Flux
						.just("0123456789")
						.repeat(20000)
						.map(value -> toDataBuffer(value, response.bufferFactory()));
				return response.writeWith(responseBody);
			}
			else if (path.endsWith("write-and-never-complete")) {
				Flux<DataBuffer> responseBody = Flux
						.just("0123456789")
						.repeat(20000)
						.map(value -> toDataBuffer(value, response.bufferFactory()))
						.mergeWith(Flux.never());
				return response.writeWith(responseBody);
			}
			return response.writeWith(Flux.empty());
		}

		private DataBuffer toDataBuffer(String value, DataBufferFactory factory) {
			byte[] data = (value).getBytes(StandardCharsets.UTF_8);
			DataBuffer buffer = factory.allocateBuffer(data.length);
			buffer.write(data);
			return buffer;
		}
	}

}
