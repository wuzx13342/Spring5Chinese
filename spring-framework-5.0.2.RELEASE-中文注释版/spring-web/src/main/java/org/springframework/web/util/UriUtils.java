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

package org.springframework.web.util;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;

/**
 * Utility class for URI encoding and decoding based on RFC 3986.
 * Offers encoding methods for the various URI components.
 *
 * <p>All {@code encode*(String, String)} methods in this class operate in a similar way:
 * <ul>
 * <li>Valid characters for the specific URI component as defined in RFC 3986 stay the same.</li>
 * <li>All other characters are converted into one or more bytes in the given encoding scheme.
 * Each of the resulting bytes is written as a hexadecimal string in the "<code>%<i>xy</i></code>"
 * format.</li>
 * </ul>
 *
 * @author Arjen Poutsma
 * @author Juergen Hoeller
 * @since 3.0
 * @see <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC 3986</a>
 */
public abstract class UriUtils {

	/**
	 * Encode the given URI scheme with the given encoding.
	 * @param scheme the scheme to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded scheme
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeScheme(String scheme, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(scheme, encoding, HierarchicalUriComponents.Type.SCHEME);
	}

	/**
	 * Encode the given URI scheme with the given encoding.
	 * @param scheme the scheme to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded scheme
	 * @since 5.0
	 */
	public static String encodeScheme(String scheme, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(scheme, charset, HierarchicalUriComponents.Type.SCHEME);
	}

	/**
	 * Encode the given URI authority with the given encoding.
	 * @param authority the authority to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded authority
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeAuthority(String authority, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(authority, encoding, HierarchicalUriComponents.Type.AUTHORITY);
	}

	/**
	 * Encode the given URI authority with the given encoding.
	 * @param authority the authority to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded authority
	 * @since 5.0
	 */
	public static String encodeAuthority(String authority, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(authority, charset, HierarchicalUriComponents.Type.AUTHORITY);
	}

	/**
	 * Encode the given URI user info with the given encoding.
	 * @param userInfo the user info to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded user info
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeUserInfo(String userInfo, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(userInfo, encoding, HierarchicalUriComponents.Type.USER_INFO);
	}

	/**
	 * Encode the given URI user info with the given encoding.
	 * @param userInfo the user info to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded user info
	 * @since 5.0
	 */
	public static String encodeUserInfo(String userInfo, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(userInfo, charset, HierarchicalUriComponents.Type.USER_INFO);
	}

	/**
	 * Encode the given URI host with the given encoding.
	 * @param host the host to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded host
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeHost(String host, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(host, encoding, HierarchicalUriComponents.Type.HOST_IPV4);
	}

	/**
	 * Encode the given URI host with the given encoding.
	 * @param host the host to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded host
	 * @since 5.0
	 */
	public static String encodeHost(String host, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(host, charset, HierarchicalUriComponents.Type.HOST_IPV4);
	}

	/**
	 * Encode the given URI port with the given encoding.
	 * @param port the port to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded port
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodePort(String port, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(port, encoding, HierarchicalUriComponents.Type.PORT);
	}

	/**
	 * Encode the given URI port with the given encoding.
	 * @param port the port to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded port
	 * @since 5.0
	 */
	public static String encodePort(String port, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(port, charset, HierarchicalUriComponents.Type.PORT);
	}

	/**
	 * Encode the given URI path with the given encoding.
	 * @param path the path to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded path
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodePath(String path, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(path, encoding, HierarchicalUriComponents.Type.PATH);
	}

	/**
	 * Encode the given URI path with the given encoding.
	 * @param path the path to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded path
	 * @since 5.0
	 */
	public static String encodePath(String path, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(path, charset, HierarchicalUriComponents.Type.PATH);
	}

	/**
	 * Encode the given URI path segment with the given encoding.
	 * @param segment the segment to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded segment
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodePathSegment(String segment, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(segment, encoding, HierarchicalUriComponents.Type.PATH_SEGMENT);
	}

	/**
	 * Encode the given URI path segment with the given encoding.
	 * @param segment the segment to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded segment
	 * @since 5.0
	 */
	public static String encodePathSegment(String segment, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(segment, charset, HierarchicalUriComponents.Type.PATH_SEGMENT);
	}

	/**
	 * Encode the given URI query with the given encoding.
	 * @param query the query to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded query
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeQuery(String query, String encoding) throws UnsupportedEncodingException {
		return HierarchicalUriComponents.encodeUriComponent(query, encoding, HierarchicalUriComponents.Type.QUERY);
	}

	/**
	 * Encode the given URI query with the given encoding.
	 * @param query the query to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded query
	 * @since 5.0
	 */
	public static String encodeQuery(String query, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(query, charset, HierarchicalUriComponents.Type.QUERY);
	}

	/**
	 * Encode the given URI query parameter with the given encoding.
	 * @param queryParam the query parameter to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded query parameter
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeQueryParam(String queryParam, String encoding)
			throws UnsupportedEncodingException {

		return HierarchicalUriComponents.encodeUriComponent(
				queryParam, encoding, HierarchicalUriComponents.Type.QUERY_PARAM);
	}

	/**
	 * Encode the given URI query parameter with the given encoding.
	 * @param queryParam the query parameter to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded query parameter
	 * @since 5.0
	 */
	public static String encodeQueryParam(String queryParam, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(
				queryParam, charset, HierarchicalUriComponents.Type.QUERY_PARAM);
	}

	/**
	 * Encode the given URI fragment with the given encoding.
	 * @param fragment the fragment to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded fragment
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encodeFragment(String fragment, String encoding)
			throws UnsupportedEncodingException {

		return HierarchicalUriComponents.encodeUriComponent(
				fragment, encoding, HierarchicalUriComponents.Type.FRAGMENT);
	}

	/**
	 * Encode the given URI fragment with the given encoding.
	 * @param fragment the fragment to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded fragment
	 * @since 5.0
	 */
	public static String encodeFragment(String fragment, Charset charset) {
		return HierarchicalUriComponents.encodeUriComponent(fragment, charset, HierarchicalUriComponents.Type.FRAGMENT);
	}


	/**
	 * Encode characters outside the unreserved character set as defined in
	 * <a href="https://tools.ietf.org/html/rfc3986#section-2">RFC 3986 Section 2</a>.
	 * <p>This can be used to ensure the given String will not contain any
	 * characters with reserved URI meaning regardless of URI component.
	 * @param source the String to be encoded
	 * @param encoding the character encoding to encode to
	 * @return the encoded String
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 */
	public static String encode(String source, String encoding) throws UnsupportedEncodingException {
		HierarchicalUriComponents.Type type = HierarchicalUriComponents.Type.URI;
		return HierarchicalUriComponents.encodeUriComponent(source, encoding, type);
	}

	/**
	 * Encode characters outside the unreserved character set as defined in
	 * <a href="https://tools.ietf.org/html/rfc3986#section-2">RFC 3986 Section 2</a>.
	 * <p>This can be used to ensure the given String will not contain any
	 * characters with reserved URI meaning regardless of URI component.
	 * @param source the String to be encoded
	 * @param charset the character encoding to encode to
	 * @return the encoded String
	 * @since 5.0
	 */
	public static String encode(String source, Charset charset) {
		HierarchicalUriComponents.Type type = HierarchicalUriComponents.Type.URI;
		return HierarchicalUriComponents.encodeUriComponent(source, charset, type);
	}

	/**
	 * Decode the given encoded URI component.
	 * <p>See {@link StringUtils#uriDecode(String, Charset)} for the decoding rules.
	 * @param source the encoded String
	 * @param encoding the character encoding to use
	 * @return the decoded value
	 * @throws IllegalArgumentException when the given source contains invalid encoded sequences
	 * @throws UnsupportedEncodingException when the given encoding parameter is not supported
	 * @see StringUtils#uriDecode(String, Charset)
	 * @see java.net.URLDecoder#decode(String, String)
	 */
	public static String decode(String source, String encoding) throws UnsupportedEncodingException {
		return StringUtils.uriDecode(source, Charset.forName(encoding));
	}

	/**
	 * Decode the given encoded URI component.
	 * <p>See {@link StringUtils#uriDecode(String, Charset)} for the decoding rules.
	 * @param source the encoded String
	 * @param charset the character encoding to use
	 * @return the decoded value
	 * @throws IllegalArgumentException when the given source contains invalid encoded sequences
	 * @since 5.0
	 * @see StringUtils#uriDecode(String, Charset)
	 * @see java.net.URLDecoder#decode(String, String)
	 */
	public static String decode(String source, Charset charset) {
		return StringUtils.uriDecode(source, charset);
	}

	/**
	 * Extract the file extension from the given URI path.
	 * @param path the URI path (e.g. "/products/index.html")
	 * @return the extracted file extension (e.g. "html")
	 * @since 4.3.2
	 */
	@Nullable
	public static String extractFileExtension(String path) {
		int end = path.indexOf('?');
		int fragmentIndex = path.indexOf('#');
		if (fragmentIndex != -1 && (end == -1 || fragmentIndex < end)) {
			end = fragmentIndex;
		}
		if (end == -1) {
			end = path.length();
		}
		int begin = path.lastIndexOf('/', end) + 1;
		int paramIndex = path.indexOf(';', begin);
		end = (paramIndex != -1 && paramIndex < end ? paramIndex : end);
		int extIndex = path.lastIndexOf('.', end);
		if (extIndex != -1 && extIndex > begin) {
			return path.substring(extIndex + 1, end);
		}
		return null;
	}


	/**
	 * Apply {@link #encode(String, String)} to the values in the given URI
	 * variables and return a new Map containing the encoded values.
	 * @param uriVariables the URI variable values to be encoded
	 * @return the encoded String
	 * @since 5.0
	 */
	public static Map<String, String> encodeUriVariables(Map<String, ?> uriVariables) {
		Map<String, String> result = new LinkedHashMap<>(uriVariables.size());
		uriVariables.forEach((key, value) -> {
			String stringValue = (value != null ? value.toString() : "");
			result.put(key, encode(stringValue, StandardCharsets.UTF_8));
		});
		return result;
	}

	/**
	 * Apply {@link #encode(String, String)} to the values in the given URI
	 * variables and return a new array containing the encoded values.
	 * @param uriVariables the URI variable values to be encoded
	 * @return the encoded String
	 * @since 5.0
	 */
	public static Object[] encodeUriVariables(Object... uriVariables) {
		return Arrays.stream(uriVariables)
				.map(value -> {
					String stringValue = (value != null ? value.toString() : "");
					return encode(stringValue, StandardCharsets.UTF_8);
				})
				.collect(Collectors.toList()).toArray();
	}

}
