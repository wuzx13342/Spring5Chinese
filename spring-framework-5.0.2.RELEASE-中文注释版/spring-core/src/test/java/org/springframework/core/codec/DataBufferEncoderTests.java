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

package org.springframework.core.codec;

import java.nio.ByteBuffer;
import java.util.Collections;

import org.junit.Test;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;

import org.springframework.core.ResolvableType;
import org.springframework.core.io.buffer.AbstractDataBufferAllocatingTestCase;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.util.MimeTypeUtils;

import static org.junit.Assert.*;

/**
 * @author Sebastien Deleuze
 */
public class DataBufferEncoderTests extends AbstractDataBufferAllocatingTestCase {

	private final DataBufferEncoder encoder = new DataBufferEncoder();

	@Test
	public void canEncode() {
		assertTrue(this.encoder.canEncode(ResolvableType.forClass(DataBuffer.class),
				MimeTypeUtils.TEXT_PLAIN));
		assertFalse(this.encoder.canEncode(ResolvableType.forClass(Integer.class),
				MimeTypeUtils.TEXT_PLAIN));
		assertTrue(this.encoder.canEncode(ResolvableType.forClass(DataBuffer.class),
				MimeTypeUtils.APPLICATION_JSON));

		// SPR-15464
		assertFalse(this.encoder.canEncode(ResolvableType.NONE, null));
	}

	@Test
	public void encode() {
		DataBuffer fooBuffer = stringBuffer("foo");
		DataBuffer barBuffer = stringBuffer("bar");
		Flux<DataBuffer> source = Flux.just(fooBuffer, barBuffer);

		Flux<DataBuffer> output = this.encoder.encode(source, this.bufferFactory,
				ResolvableType.forClassWithGenerics(Publisher.class, ByteBuffer.class),
				null, Collections.emptyMap());

		assertSame(source, output);

		release(fooBuffer, barBuffer);
	}

}
