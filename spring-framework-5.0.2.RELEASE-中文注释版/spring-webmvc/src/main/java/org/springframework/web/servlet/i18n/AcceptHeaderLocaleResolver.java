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

package org.springframework.web.servlet.i18n;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.lang.Nullable;
import org.springframework.web.servlet.LocaleResolver;

/**
 * {@link LocaleResolver} implementation that simply uses the primary locale
 * specified in the "accept-language" header of the HTTP request (that is,
 * the locale sent by the client browser, normally that of the client's OS).
 *
 * <p>Note: Does not support {@code setLocale}, since the accept header
 * can only be changed through changing the client's locale settings.
 *
 * @author Juergen Hoeller
 * @author Rossen Stoyanchev
 * @since 27.02.2003
 * @see javax.servlet.http.HttpServletRequest#getLocale()
 */
public class AcceptHeaderLocaleResolver implements LocaleResolver {

	private final List<Locale> supportedLocales = new ArrayList<>(4);

	@Nullable
	private Locale defaultLocale;


	/**
	 * Configure supported locales to check against the requested locales
	 * determined via {@link HttpServletRequest#getLocales()}. If this is not
	 * configured then {@link HttpServletRequest#getLocale()} is used instead.
	 * @param locales the supported locales
	 * @since 4.3
	 */
	public void setSupportedLocales(@Nullable List<Locale> locales) {
		this.supportedLocales.clear();
		if (locales != null) {
			this.supportedLocales.addAll(locales);
		}
	}

	/**
	 * Return the configured list of supported locales.
	 * @since 4.3
	 */
	public List<Locale> getSupportedLocales() {
		return this.supportedLocales;
	}

	/**
	 * Configure a fixed default locale to fall back on if the request does not
	 * have an "Accept-Language" header.
	 * <p>By default this is not set in which case when there is "Accept-Language"
	 * header, the default locale for the server is used as defined in
	 * {@link HttpServletRequest#getLocale()}.
	 * @param defaultLocale the default locale to use
	 * @since 4.3
	 */
	public void setDefaultLocale(@Nullable Locale defaultLocale) {
		this.defaultLocale = defaultLocale;
	}

	/**
	 * The configured default locale, if any.
	 * @since 4.3
	 */
	@Nullable
	public Locale getDefaultLocale() {
		return this.defaultLocale;
	}


	@Override
	public Locale resolveLocale(HttpServletRequest request) {
		Locale defaultLocale = getDefaultLocale();
		if (defaultLocale != null && request.getHeader("Accept-Language") == null) {
			return defaultLocale;
		}
		Locale requestLocale = request.getLocale();
		if (isSupportedLocale(requestLocale)) {
			return requestLocale;
		}
		Locale supportedLocale = findSupportedLocale(request);
		if (supportedLocale != null) {
			return supportedLocale;
		}
		return (defaultLocale != null ? defaultLocale : requestLocale);
	}

	private boolean isSupportedLocale(Locale locale) {
		List<Locale> supportedLocales = getSupportedLocales();
		return (supportedLocales.isEmpty() || supportedLocales.contains(locale));
	}

	@Nullable
	private Locale findSupportedLocale(HttpServletRequest request) {
		Enumeration<Locale> requestLocales = request.getLocales();
		while (requestLocales.hasMoreElements()) {
			Locale locale = requestLocales.nextElement();
			if (getSupportedLocales().contains(locale)) {
				return locale;
			}
		}
		return null;
	}

	@Override
	public void setLocale(HttpServletRequest request, @Nullable HttpServletResponse response, @Nullable Locale locale) {
		throw new UnsupportedOperationException(
				"Cannot change HTTP accept header - use a different locale resolution strategy");
	}

}
