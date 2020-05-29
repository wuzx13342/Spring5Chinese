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

package org.springframework.web.socket.config.annotation;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.lang.Nullable;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.util.MultiValueMap;
import org.springframework.web.HttpRequestHandler;
import org.springframework.web.servlet.handler.AbstractHandlerMapping;
import org.springframework.web.servlet.handler.SimpleUrlHandlerMapping;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.support.WebSocketHandlerMapping;
import org.springframework.web.util.UrlPathHelper;

/**
 * A {@link WebSocketHandlerRegistry} that maps {@link WebSocketHandler}s to URLs for use
 * in a Servlet container.
 *
 * @author Rossen Stoyanchev
 * @since 4.0
 */
public class ServletWebSocketHandlerRegistry implements WebSocketHandlerRegistry {

	private final List<ServletWebSocketHandlerRegistration> registrations = new ArrayList<>(4);

	@Nullable
	private TaskScheduler scheduler;

	private int order = 1;

	@Nullable
	private UrlPathHelper urlPathHelper;


	public ServletWebSocketHandlerRegistry() {
	}

	/**
	 * Deprecated constructor with a TaskScheduler for SockJS use.
	 * @deprecated as of 5.0 a TaskScheduler is not provided upfront, not until
	 * it is obvious that it is needed, see {@link #requiresTaskScheduler()} and
	 * {@link #setTaskScheduler}.
	 */
	@Deprecated
	public ServletWebSocketHandlerRegistry(ThreadPoolTaskScheduler scheduler) {
		this.scheduler = scheduler;
	}


	@Override
	public WebSocketHandlerRegistration addHandler(WebSocketHandler handler, String... paths) {
		ServletWebSocketHandlerRegistration registration = new ServletWebSocketHandlerRegistration();
		registration.addHandler(handler, paths);
		this.registrations.add(registration);
		return registration;
	}

	/**
	 * Set the order for the resulting {@link SimpleUrlHandlerMapping} relative to
	 * other handler mappings configured in Spring MVC.
	 * <p>The default value is 1.
	 */
	public void setOrder(int order) {
		this.order = order;
	}

	public int getOrder() {
		return this.order;
	}

	/**
	 * Set the UrlPathHelper to configure on the {@code SimpleUrlHandlerMapping}
	 * used to map handshake requests.
	 */
	public void setUrlPathHelper(@Nullable UrlPathHelper urlPathHelper) {
		this.urlPathHelper = urlPathHelper;
	}

	@Nullable
	public UrlPathHelper getUrlPathHelper() {
		return this.urlPathHelper;
	}


	/**
	 * Whether there are any endpoint SockJS registrations without a TaskScheduler.
	 * This method should be invoked just before {@link #getHandlerMapping()} to
	 * allow for registrations to be made first.
	 */
	protected boolean requiresTaskScheduler() {
		return this.registrations.stream()
				.anyMatch(r -> r.getSockJsServiceRegistration() != null &&
						r.getSockJsServiceRegistration().getTaskScheduler() == null);
	}

	/**
	 * Configure a TaskScheduler for SockJS endpoints. This should be configured
	 * before calling {@link #getHandlerMapping()} after checking if
	 * {@link #requiresTaskScheduler()} returns {@code true}.
	 */
	protected void setTaskScheduler(TaskScheduler scheduler) {
		this.scheduler = scheduler;
	}

	public AbstractHandlerMapping getHandlerMapping() {
		Map<String, Object> urlMap = new LinkedHashMap<>();
		for (ServletWebSocketHandlerRegistration registration : this.registrations) {
			updateTaskScheduler(registration);
			MultiValueMap<HttpRequestHandler, String> mappings = registration.getMappings();
			for (HttpRequestHandler httpHandler : mappings.keySet()) {
				for (String pattern : mappings.get(httpHandler)) {
					urlMap.put(pattern, httpHandler);
				}
			}
		}
		WebSocketHandlerMapping hm = new WebSocketHandlerMapping();
		hm.setUrlMap(urlMap);
		hm.setOrder(this.order);
		if (this.urlPathHelper != null) {
			hm.setUrlPathHelper(this.urlPathHelper);
		}
		return hm;
	}

	private void updateTaskScheduler(ServletWebSocketHandlerRegistration registration) {
		SockJsServiceRegistration sockJsRegistration = registration.getSockJsServiceRegistration();
		if (sockJsRegistration != null && this.scheduler != null && sockJsRegistration.getTaskScheduler() == null) {
			sockJsRegistration.setTaskScheduler(this.scheduler);
		}
	}

}
