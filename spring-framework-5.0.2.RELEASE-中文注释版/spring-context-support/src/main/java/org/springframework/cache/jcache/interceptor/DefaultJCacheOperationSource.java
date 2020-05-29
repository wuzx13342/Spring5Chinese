/*
 * Copyright 2002-2015 the original author or authors.
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

package org.springframework.cache.jcache.interceptor;

import java.util.Collection;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.NoUniqueBeanDefinitionException;
import org.springframework.beans.factory.SmartInitializingSingleton;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.cache.interceptor.CacheOperationInvocationContext;
import org.springframework.cache.interceptor.CacheResolver;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.cache.interceptor.SimpleCacheResolver;
import org.springframework.cache.interceptor.SimpleKeyGenerator;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;

/**
 * The default {@link JCacheOperationSource} implementation delegating
 * default operations to configurable services with sensible defaults
 * when not present.
 *
 * @author Stephane Nicoll
 * @since 4.1
 */
public class DefaultJCacheOperationSource extends AnnotationJCacheOperationSource
		implements BeanFactoryAware, InitializingBean, SmartInitializingSingleton {

	private CacheManager cacheManager;

	private CacheResolver cacheResolver;

	private CacheResolver exceptionCacheResolver;

	private KeyGenerator keyGenerator = new SimpleKeyGenerator();

	private KeyGenerator adaptedKeyGenerator;

	private BeanFactory beanFactory;


	/**
	 * Set the default {@link CacheManager} to use to lookup cache by name. Only mandatory
	 * if the {@linkplain CacheResolver cache resolvers} have not been set.
	 */
	public void setCacheManager(@Nullable CacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}

	/**
	 * Return the specified cache manager to use, if any.
	 */
	@Nullable
	public CacheManager getCacheManager() {
		return this.cacheManager;
	}

	/**
	 * Set the {@link CacheResolver} to resolve regular caches. If none is set, a default
	 * implementation using the specified cache manager will be used.
	 */
	public void setCacheResolver(@Nullable CacheResolver cacheResolver) {
		this.cacheResolver = cacheResolver;
	}

	/**
	 * Return the specified cache resolver to use, if any.
	 */
	@Nullable
	public CacheResolver getCacheResolver() {
		return this.cacheResolver;
	}

	/**
	 * Set the {@link CacheResolver} to resolve exception caches. If none is set, a default
	 * implementation using the specified cache manager will be used.
	 */
	public void setExceptionCacheResolver(@Nullable CacheResolver exceptionCacheResolver) {
		this.exceptionCacheResolver = exceptionCacheResolver;
	}

	/**
	 * Return the specified exception cache resolver to use, if any.
	 */
	@Nullable
	public CacheResolver getExceptionCacheResolver() {
		return this.exceptionCacheResolver;
	}

	/**
	 * Set the default {@link KeyGenerator}. If none is set, a {@link SimpleKeyGenerator}
	 * honoring the JSR-107 {@link javax.cache.annotation.CacheKey} and
	 * {@link javax.cache.annotation.CacheValue} will be used.
	 */
	public void setKeyGenerator(@Nullable KeyGenerator keyGenerator) {
		this.keyGenerator = keyGenerator;
	}

	/**
	 * Return the specified key generator to use, if any.
	 */
	@Nullable
	public KeyGenerator getKeyGenerator() {
		return this.keyGenerator;
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) {
		this.beanFactory = beanFactory;
	}


	@Override
	public void afterPropertiesSet() {
		this.adaptedKeyGenerator = new KeyGeneratorAdapter(this, this.keyGenerator);
	}

	@Override
	public void afterSingletonsInstantiated() {
		// Make sure that the cache resolver is initialized. An exception cache resolver is only
		// required if the exceptionCacheName attribute is set on an operation
		Assert.notNull(getDefaultCacheResolver(), "Cache resolver should have been initialized");
	}


	@Override
	protected <T> T getBean(Class<T> type) {
		try {
			return this.beanFactory.getBean(type);
		}
		catch (NoUniqueBeanDefinitionException ex) {
			throw new IllegalStateException("No unique [" + type.getName() + "] bean found in application context - " +
					"mark one as primary, or declare a more specific implementation type for your cache", ex);
		}
		catch (NoSuchBeanDefinitionException ex) {
			if (logger.isDebugEnabled()) {
				logger.debug("No bean of type [" + type.getName() + "] found in application context", ex);
			}
			return BeanUtils.instantiateClass(type);
		}
	}

	protected CacheManager getDefaultCacheManager() {
		if (this.cacheManager == null) {
			try {
				this.cacheManager = this.beanFactory.getBean(CacheManager.class);
			}
			catch (NoUniqueBeanDefinitionException ex) {
				throw new IllegalStateException("No unique bean of type CacheManager found. "+
						"Mark one as primary or declare a specific CacheManager to use.");
			}
			catch (NoSuchBeanDefinitionException ex) {
				throw new IllegalStateException("No bean of type CacheManager found. Register a CacheManager "+
						"bean or remove the @EnableCaching annotation from your configuration.");
			}
		}
		return this.cacheManager;
	}

	@Override
	protected CacheResolver getDefaultCacheResolver() {
		if (this.cacheResolver == null) {
			this.cacheResolver = new SimpleCacheResolver(getDefaultCacheManager());
		}
		return this.cacheResolver;
	}

	@Override
	protected CacheResolver getDefaultExceptionCacheResolver() {
		if (this.exceptionCacheResolver == null) {
			this.exceptionCacheResolver = new LazyCacheResolver();
		}
		return this.exceptionCacheResolver;
	}

	@Override
	protected KeyGenerator getDefaultKeyGenerator() {
		return this.adaptedKeyGenerator;
	}


	/**
	 * Only resolve the default exception cache resolver when an exception needs to be handled.
	 * <p>A non-JSR-107 setup requires either a {@link CacheManager} or a {@link CacheResolver}. If only
	 * the latter is specified, it is not possible to extract a default exception {@code CacheResolver}
	 * from a custom {@code CacheResolver} implementation so we have to fallback on the {@code CacheManager}.
	 * <p>This gives this weird situation of a perfectly valid configuration that breaks all the sudden
	 * because the JCache support is enabled. To avoid this we resolve the default exception {@code CacheResolver}
	 * as late as possible to avoid such hard requirement in other cases.
	 */
	class LazyCacheResolver implements CacheResolver {

		private CacheResolver cacheResolver;

		@Override
		public Collection<? extends Cache> resolveCaches(CacheOperationInvocationContext<?> context) {
			if (this.cacheResolver == null) {
				this.cacheResolver = new SimpleExceptionCacheResolver(getDefaultCacheManager());
			}
			return this.cacheResolver.resolveCaches(context);
		}
	}

}
