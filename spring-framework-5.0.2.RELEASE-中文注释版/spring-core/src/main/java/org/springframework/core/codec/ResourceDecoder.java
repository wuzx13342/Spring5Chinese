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

import java.io.ByteArrayInputStream;
import java.util.Map;

import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import org.springframework.core.ResolvableType;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.MimeType;
import org.springframework.util.MimeTypeUtils;

/**
 * Decoder for {@link Resource}s.
 *
 * @author Arjen Poutsma
 * @since 5.0
 */
public class ResourceDecoder extends AbstractDecoder<Resource> {

	public ResourceDecoder() {
		super(MimeTypeUtils.ALL);
	}


	@Override
	public boolean canDecode(ResolvableType elementType, @Nullable MimeType mimeType) {
		Class<?> clazz = elementType.getRawClass();
		return (clazz != null &&
				(InputStreamResource.class == clazz || clazz.isAssignableFrom(ByteArrayResource.class)) &&
				super.canDecode(elementType, mimeType));
	}

	@Override
	public Flux<Resource> decode(Publisher<DataBuffer> inputStream, ResolvableType elementType,
			@Nullable MimeType mimeType, @Nullable Map<String, Object> hints) {

		return Flux.from(decodeToMono(inputStream, elementType, mimeType, hints));
	}

	@Override
	public Mono<Resource> decodeToMono(Publisher<DataBuffer> inputStream, ResolvableType elementType,
			@Nullable MimeType mimeType, @Nullable Map<String, Object> hints) {

		Class<?> clazz = elementType.getRawClass();
		Assert.state(clazz != null, "No resource class");

		Mono<byte[]> byteArray = Flux.from(inputStream).
				reduce(DataBuffer::write).
				map(dataBuffer -> {
					byte[] bytes = new byte[dataBuffer.readableByteCount()];
					dataBuffer.read(bytes);
					DataBufferUtils.release(dataBuffer);
					return bytes;
				});


		if (InputStreamResource.class == clazz) {
			return Mono.from(byteArray.map(ByteArrayInputStream::new).map(InputStreamResource::new));
		}
		else if (clazz.isAssignableFrom(ByteArrayResource.class)) {
			return Mono.from(byteArray.map(ByteArrayResource::new));
		}
		else {
			return Mono.error(new IllegalStateException("Unsupported resource class: " + clazz));
		}
	}

}
