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

package org.springframework.web.reactive.socket.server.upgrade;

import java.security.Principal;

import reactor.core.publisher.Mono;
import reactor.ipc.netty.http.server.HttpServerResponse;

import org.springframework.core.io.buffer.NettyDataBufferFactory;
import org.springframework.http.server.reactive.AbstractServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.lang.Nullable;
import org.springframework.web.reactive.socket.HandshakeInfo;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.adapter.ReactorNettyWebSocketSession;
import org.springframework.web.reactive.socket.server.RequestUpgradeStrategy;
import org.springframework.web.server.ServerWebExchange;

/**
 * A {@link RequestUpgradeStrategy} for use with Reactor Netty.
 *
 * @author Rossen Stoyanchev
 * @since 5.0
 */
public class ReactorNettyRequestUpgradeStrategy implements RequestUpgradeStrategy {

	@Override
	public Mono<Void> upgrade(ServerWebExchange exchange, WebSocketHandler handler, @Nullable String subProtocol) {
		ServerHttpResponse response = exchange.getResponse();
		HttpServerResponse nativeResponse = ((AbstractServerHttpResponse) response).getNativeResponse();
		HandshakeInfo info = getHandshakeInfo(exchange, subProtocol);
		NettyDataBufferFactory bufferFactory = (NettyDataBufferFactory) response.bufferFactory();

		return nativeResponse.sendWebsocket(subProtocol,
				(in, out) -> handler.handle(new ReactorNettyWebSocketSession(in, out, info, bufferFactory)));
	}

	private HandshakeInfo getHandshakeInfo(ServerWebExchange exchange, @Nullable String protocol) {
		ServerHttpRequest request = exchange.getRequest();
		Mono<Principal> principal = exchange.getPrincipal();
		return new HandshakeInfo(request.getURI(), request.getHeaders(), principal, protocol);
	}

}
