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

package org.springframework.core.codec;

import java.io.IOException;
import java.util.Collections;

import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import org.springframework.core.ResolvableType;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.buffer.AbstractDataBufferAllocatingTestCase;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.util.MimeTypeUtils;
import org.springframework.util.StreamUtils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * @author Arjen Poutsma
 */
public class ResourceDecoderTests extends AbstractDataBufferAllocatingTestCase {

	private final ResourceDecoder decoder = new ResourceDecoder();

	@Test
	public void canDecode() throws Exception {
		assertTrue(this.decoder.canDecode(ResolvableType.forClass(InputStreamResource.class),
				MimeTypeUtils.TEXT_PLAIN));
		assertTrue(this.decoder.canDecode(ResolvableType.forClass(ByteArrayResource.class),
				MimeTypeUtils.TEXT_PLAIN));
		assertTrue(this.decoder.canDecode(ResolvableType.forClass(Resource.class),
				MimeTypeUtils.TEXT_PLAIN));
		assertTrue(this.decoder.canDecode(ResolvableType.forClass(InputStreamResource.class),
				MimeTypeUtils.APPLICATION_JSON));
	}

	@Test
	public void decode() throws Exception {
		DataBuffer fooBuffer = stringBuffer("foo");
		DataBuffer barBuffer = stringBuffer("bar");
		Flux<DataBuffer> source = Flux.just(fooBuffer, barBuffer);

		Flux<Resource> result = this.decoder
				.decode(source, ResolvableType.forClass(Resource.class), null, Collections.emptyMap());

		StepVerifier.create(result)
				.consumeNextWith(resource -> {
					try {
						byte[] bytes = StreamUtils.copyToByteArray(resource.getInputStream());
						assertEquals("foobar", new String(bytes));
					}
					catch (IOException e) {
						fail(e.getMessage());
					}
				})
				.expectComplete()
				.verify();
	}

}