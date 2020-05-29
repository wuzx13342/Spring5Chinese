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

package org.springframework.web.reactive.function.client;

import java.util.List;
import java.util.Optional;
import java.util.OptionalLong;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.reactive.ClientHttpResponse;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyExtractor;

/**
 * Represents an HTTP response, as returned by {@link WebClient} and also
 * {@link ExchangeFunction}. Provides access to the response status and headers,
 * and also methods to consume the response body.
 *
 * <p><strong>NOTE:</strong> When given access to a {@link ClientResponse},
 * through the {@code WebClient}
 * {@link WebClient.RequestHeadersSpec#exchange() exchange()} method,
 * you must always use one of the body or toEntity methods to ensure resources
 * are released and avoid potential issues with HTTP connection pooling.
 * You can use {@code bodyToMono(Void.class)} if no response content is
 * expected. However keep in mind that if the response does have content, the
 * connection will be closed and will not be placed back in the pool.
 *
 * @author Brian Clozel
 * @author Arjen Poutsma
 * @since 5.0
 */
public interface ClientResponse {

	/**
	 * Return the status code of this response.
	 */
	HttpStatus statusCode();

	/**
	 * Return the headers of this response.
	 */
	Headers headers();

	/**
	 * Return cookies of this response.
	 */
	MultiValueMap<String, ResponseCookie> cookies();

	/**
	 * Extract the body with the given {@code BodyExtractor}.
	 * @param extractor the {@code BodyExtractor} that reads from the response
	 * @param <T> the type of the body returned
	 * @return the extracted body
	 */
	<T> T body(BodyExtractor<T, ? super ClientHttpResponse> extractor);

	/**
	 * Extract the body to a {@code Mono}.
	 * @param elementClass the class of element in the {@code Mono}
	 * @param <T> the element type
	 * @return a mono containing the body of the given type {@code T}
	 */
	<T> Mono<T> bodyToMono(Class<? extends T> elementClass);

	/**
	 * Extract the body to a {@code Mono}.
	 * @param typeReference a type reference describing the expected response body type
	 * @param <T> the element type
	 * @return a mono containing the body of the given type {@code T}
	 */
	<T> Mono<T> bodyToMono(ParameterizedTypeReference<T> typeReference);

	/**
	 * Extract the body to a {@code Flux}.
	 * @param elementClass the class of element in the {@code Flux}
	 * @param <T> the element type
	 * @return a flux containing the body of the given type {@code T}
	 */
	<T> Flux<T> bodyToFlux(Class<? extends T> elementClass);

	/**
	 * Extract the body to a {@code Flux}.
	 * @param typeReference a type reference describing the expected response body type
	 * @param <T> the element type
	 * @return a flux containing the body of the given type {@code T}
	 */
	<T> Flux<T> bodyToFlux(ParameterizedTypeReference<T> typeReference);

	/**
	 * Return this response as a delayed {@code ResponseEntity}.
	 * @param bodyType the expected response body type
	 * @param <T> response body type
	 * @return {@code Mono} with the {@code ResponseEntity}
	 */
	<T> Mono<ResponseEntity<T>> toEntity(Class<T> bodyType);

	/**
	 * Return this response as a delayed {@code ResponseEntity}.
	 * @param typeReference a type reference describing the expected response body type
	 * @param <T> response body type
	 * @return {@code Mono} with the {@code ResponseEntity}
	 */
	<T> Mono<ResponseEntity<T>> toEntity(ParameterizedTypeReference<T> typeReference);

	/**
	 * Return this response as a delayed list of {@code ResponseEntity}s.
	 * @param elementType the expected response body list element type
	 * @param <T> the type of elements in the list
	 * @return {@code Mono} with the list of {@code ResponseEntity}s
	 */
	<T> Mono<ResponseEntity<List<T>>> toEntityList(Class<T> elementType);

	/**
	 * Return this response as a delayed list of {@code ResponseEntity}s.
	 * @param typeReference a type reference describing the expected response body type
	 * @param <T> the type of elements in the list
	 * @return {@code Mono} with the list of {@code ResponseEntity}s
	 */
	<T> Mono<ResponseEntity<List<T>>> toEntityList(ParameterizedTypeReference<T> typeReference);


	/**
	 * Represents the headers of the HTTP response.
	 * @see ClientResponse#headers()
	 */
	interface Headers {

		/**
		 * Return the length of the body in bytes, as specified by the
		 * {@code Content-Length} header.
		 */
		OptionalLong contentLength();

		/**
		 * Return the {@linkplain MediaType media type} of the body, as specified
		 * by the {@code Content-Type} header.
		 */
		Optional<MediaType> contentType();

		/**
		 * Return the header value(s), if any, for the header of the given name.
		 * <p>Return an empty list if no header values are found.
		 * @param headerName the header name
		 */
		List<String> header(String headerName);

		/**
		 * Return the headers as a {@link HttpHeaders} instance.
		 */
		HttpHeaders asHttpHeaders();
	}

}
