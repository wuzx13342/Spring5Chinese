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

package org.springframework.orm.jpa.support;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.factory.*;
import org.springframework.beans.factory.annotation.InjectionMetadata;
import org.springframework.beans.factory.config.*;
import org.springframework.beans.factory.support.MergedBeanDefinitionPostProcessor;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.core.BridgeMethodResolver;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;
import org.springframework.jndi.JndiLocatorDelegate;
import org.springframework.jndi.JndiTemplate;
import org.springframework.lang.Nullable;
import org.springframework.orm.jpa.*;
import org.springframework.util.*;

import javax.persistence.*;
import java.beans.PropertyDescriptor;
import java.io.Serializable;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

/**
 * BeanPostProcessor that processes {@link javax.persistence.PersistenceUnit}
 * and {@link javax.persistence.PersistenceContext} annotations, for injection of
 * the corresponding JPA resources {@link javax.persistence.EntityManagerFactory}
 * and {@link javax.persistence.EntityManager}. Any such annotated fields or methods
 * in any Spring-managed object will automatically be injected.
 *
 * <p>This post-processor will inject sub-interfaces of {@code EntityManagerFactory}
 * and {@code EntityManager} if the annotated fields or methods are declared as such.
 * The actual type will be verified early, with the exception of a shared ("transactional")
 * {@code EntityManager} reference, where type mismatches might be detected as late
 * as on the first actual invocation.
 *
 * <p>Note: In the present implementation, PersistenceAnnotationBeanPostProcessor
 * only supports {@code @PersistenceUnit} and {@code @PersistenceContext}
 * with the "unitName" attribute, or no attribute at all (for the default unit).
 * If those annotations are present with the "name" attribute at the class level,
 * they will simply be ignored, since those only serve as deployment hint
 * (as per the Java EE specification).
 *
 * <p>This post-processor can either obtain EntityManagerFactory beans defined
 * in the Spring application context (the default), or obtain EntityManagerFactory
 * references from JNDI ("persistence unit references"). In the bean case,
 * the persistence unit name will be matched against the actual deployed unit,
 * with the bean name used as fallback unit name if no deployed name found.
 * Typically, Spring's {@link org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean}
 * will be used for setting up such EntityManagerFactory beans. Alternatively,
 * such beans may also be obtained from JNDI, e.g. using the {@code jee:jndi-lookup}
 * XML configuration element (with the bean name matching the requested unit name).
 * In both cases, the post-processor definition will look as simple as this:
 *
 * <pre class="code">
 * &lt;bean class="org.springframework.orm.jpa.support.PersistenceAnnotationBeanPostProcessor"/&gt;</pre>
 *
 * In the JNDI case, specify the corresponding JNDI names in this post-processor's
 * {@link #setPersistenceUnits "persistenceUnits" map}, typically with matching
 * {@code persistence-unit-ref} entries in the Java EE deployment descriptor.
 * By default, those names are considered as resource references (according to the
 * Java EE resource-ref convention), located underneath the "java:comp/env/" namespace.
 * For example:
 *
 * <pre class="code">
 * &lt;bean class="org.springframework.orm.jpa.support.PersistenceAnnotationBeanPostProcessor"&gt;
 *   &lt;property name="persistenceUnits"&gt;
 *     &lt;map/gt;
 *       &lt;entry key="unit1" value="persistence/unit1"/&gt;
 *       &lt;entry key="unit2" value="persistence/unit2"/&gt;
 *     &lt;/map/gt;
 *   &lt;/property&gt;
 * &lt;/bean&gt;</pre>
 *
 * In this case, the specified persistence units will always be resolved in JNDI
 * rather than as Spring-defined beans. The entire persistence unit deployment,
 * including the weaving of persistent classes, is then up to the Java EE server.
 * Persistence contexts (i.e. EntityManager references) will be built based on
 * those server-provided EntityManagerFactory references, using Spring's own
 * transaction synchronization facilities for transactional EntityManager handling
 * (typically with Spring's {@code @Transactional} annotation for demarcation
 * and {@link org.springframework.transaction.jta.JtaTransactionManager} as backend).
 *
 * <p>If you prefer the Java EE server's own EntityManager handling, specify entries
 * in this post-processor's {@link #setPersistenceContexts "persistenceContexts" map}
 * (or {@link #setExtendedPersistenceContexts "extendedPersistenceContexts" map},
 * typically with matching {@code persistence-context-ref} entries in the
 * Java EE deployment descriptor. For example:
 *
 * <pre class="code">
 * &lt;bean class="org.springframework.orm.jpa.support.PersistenceAnnotationBeanPostProcessor"&gt;
 *   &lt;property name="persistenceContexts"&gt;
 *     &lt;map/gt;
 *       &lt;entry key="unit1" value="persistence/context1"/&gt;
 *       &lt;entry key="unit2" value="persistence/context2"/&gt;
 *     &lt;/map/gt;
 *   &lt;/property&gt;
 * &lt;/bean&gt;</pre>
 *
 * If the application only obtains EntityManager references in the first place,
 * this is all you need to specify. If you need EntityManagerFactory references
 * as well, specify entries for both "persistenceUnits" and "persistenceContexts",
 * pointing to matching JNDI locations.
 *
 * <p><b>NOTE: In general, do not inject EXTENDED EntityManagers into STATELESS beans,
 * i.e. do not use {@code @PersistenceContext} with type {@code EXTENDED} in
 * Spring beans defined with scope 'singleton' (Spring's default scope).</b>
 * Extended EntityManagers are <i>not</i> thread-safe, hence they must not be used
 * in concurrently accessed beans (which Spring-managed singletons usually are).
 *
 * <p>Note: A default PersistenceAnnotationBeanPostProcessor will be registered
 * by the "context:annotation-config" and "context:component-scan" XML tags.
 * Remove or turn off the default annotation configuration there if you intend
 * to specify a custom PersistenceAnnotationBeanPostProcessor bean definition.
 *
 * @author Rod Johnson
 * @author Juergen Hoeller
 * @since 2.0
 * @see javax.persistence.PersistenceUnit
 * @see javax.persistence.PersistenceContext
 */
@SuppressWarnings("serial")
public class PersistenceAnnotationBeanPostProcessor
		implements InstantiationAwareBeanPostProcessor, DestructionAwareBeanPostProcessor,
		MergedBeanDefinitionPostProcessor, PriorityOrdered, BeanFactoryAware, Serializable {

	@Nullable
	private Object jndiEnvironment;

	private boolean resourceRef = true;

	@Nullable
	private transient Map<String, String> persistenceUnits;

	@Nullable
	private transient Map<String, String> persistenceContexts;

	@Nullable
	private transient Map<String, String> extendedPersistenceContexts;

	private transient String defaultPersistenceUnitName = "";

	private int order = Ordered.LOWEST_PRECEDENCE - 4;

	@Nullable
	private transient ListableBeanFactory beanFactory;

	private transient final Map<String, InjectionMetadata> injectionMetadataCache = new ConcurrentHashMap<>(256);

	private final Map<Object, EntityManager> extendedEntityManagersToClose = new ConcurrentHashMap<>(16);


	/**
	 * Set the JNDI template to use for JNDI lookups.
	 * @see org.springframework.jndi.JndiAccessor#setJndiTemplate
	 */
	public void setJndiTemplate(Object jndiTemplate) {
		this.jndiEnvironment = jndiTemplate;
	}

	/**
	 * Set the JNDI environment to use for JNDI lookups.
	 * @see org.springframework.jndi.JndiAccessor#setJndiEnvironment
	 */
	public void setJndiEnvironment(Properties jndiEnvironment) {
		this.jndiEnvironment = jndiEnvironment;
	}

	/**
	 * Set whether the lookup occurs in a Java EE container, i.e. if the prefix
	 * "java:comp/env/" needs to be added if the JNDI name doesn't already
	 * contain it. PersistenceAnnotationBeanPostProcessor's default is "true".
	 * @see org.springframework.jndi.JndiLocatorSupport#setResourceRef
	 */
	public void setResourceRef(boolean resourceRef) {
		this.resourceRef = resourceRef;
	}

	/**
	 * Specify the persistence units for EntityManagerFactory lookups,
	 * as a Map from persistence unit name to persistence unit JNDI name
	 * (which needs to resolve to an EntityManagerFactory instance).
	 * <p>JNDI names specified here should refer to {@code persistence-unit-ref}
	 * entries in the Java EE deployment descriptor, matching the target persistence unit.
	 * <p>In case of no unit name specified in the annotation, the specified value
	 * for the {@link #setDefaultPersistenceUnitName default persistence unit}
	 * will be taken (by default, the value mapped to the empty String),
	 * or simply the single persistence unit if there is only one.
	 * <p>This is mainly intended for use in a Java EE environment, with all lookup
	 * driven by the standard JPA annotations, and all EntityManagerFactory
	 * references obtained from JNDI. No separate EntityManagerFactory bean
	 * definitions are necessary in such a scenario.
	 * <p>If no corresponding "persistenceContexts"/"extendedPersistenceContexts"
	 * are specified, {@code @PersistenceContext} will be resolved to
	 * EntityManagers built on top of the EntityManagerFactory defined here.
	 * Note that those will be Spring-managed EntityManagers, which implement
	 * transaction synchronization based on Spring's facilities.
	 * If you prefer the Java EE server's own EntityManager handling,
	 * specify corresponding "persistenceContexts"/"extendedPersistenceContexts".
	 */
	public void setPersistenceUnits(Map<String, String> persistenceUnits) {
		this.persistenceUnits = persistenceUnits;
	}

	/**
	 * Specify the <i>transactional</i> persistence contexts for EntityManager lookups,
	 * as a Map from persistence unit name to persistence context JNDI name
	 * (which needs to resolve to an EntityManager instance).
	 * <p>JNDI names specified here should refer to {@code persistence-context-ref}
	 * entries in the Java EE deployment descriptors, matching the target persistence unit
	 * and being set up with persistence context type {@code Transaction}.
	 * <p>In case of no unit name specified in the annotation, the specified value
	 * for the {@link #setDefaultPersistenceUnitName default persistence unit}
	 * will be taken (by default, the value mapped to the empty String),
	 * or simply the single persistence unit if there is only one.
	 * <p>This is mainly intended for use in a Java EE environment, with all
	 * lookup driven by the standard JPA annotations, and all EntityManager
	 * references obtained from JNDI. No separate EntityManagerFactory bean
	 * definitions are necessary in such a scenario, and all EntityManager
	 * handling is done by the Java EE server itself.
	 */
	public void setPersistenceContexts(Map<String, String> persistenceContexts) {
		this.persistenceContexts = persistenceContexts;
	}

	/**
	 * Specify the <i>extended</i> persistence contexts for EntityManager lookups,
	 * as a Map from persistence unit name to persistence context JNDI name
	 * (which needs to resolve to an EntityManager instance).
	 * <p>JNDI names specified here should refer to {@code persistence-context-ref}
	 * entries in the Java EE deployment descriptors, matching the target persistence unit
	 * and being set up with persistence context type {@code Extended}.
	 * <p>In case of no unit name specified in the annotation, the specified value
	 * for the {@link #setDefaultPersistenceUnitName default persistence unit}
	 * will be taken (by default, the value mapped to the empty String),
	 * or simply the single persistence unit if there is only one.
	 * <p>This is mainly intended for use in a Java EE environment, with all
	 * lookup driven by the standard JPA annotations, and all EntityManager
	 * references obtained from JNDI. No separate EntityManagerFactory bean
	 * definitions are necessary in such a scenario, and all EntityManager
	 * handling is done by the Java EE server itself.
	 */
	public void setExtendedPersistenceContexts(Map<String, String> extendedPersistenceContexts) {
		this.extendedPersistenceContexts = extendedPersistenceContexts;
	}

	/**
	 * Specify the default persistence unit name, to be used in case
	 * of no unit name specified in an {@code @PersistenceUnit} /
	 * {@code @PersistenceContext} annotation.
	 * <p>This is mainly intended for lookups in the application context,
	 * indicating the target persistence unit name (typically matching
	 * the bean name), but also applies to lookups in the
	 * {@link #setPersistenceUnits "persistenceUnits"} /
	 * {@link #setPersistenceContexts "persistenceContexts"} /
	 * {@link #setExtendedPersistenceContexts "extendedPersistenceContexts"} map,
	 * avoiding the need for duplicated mappings for the empty String there.
	 * <p>Default is to check for a single EntityManagerFactory bean
	 * in the Spring application context, if any. If there are multiple
	 * such factories, either specify this default persistence unit name
	 * or explicitly refer to named persistence units in your annotations.
	 */
	public void setDefaultPersistenceUnitName(@Nullable String unitName) {
		this.defaultPersistenceUnitName = (unitName != null ? unitName : "");
	}

	public void setOrder(int order) {
	  this.order = order;
	}

	@Override
	public int getOrder() {
	  return this.order;
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) {
		if (beanFactory instanceof ListableBeanFactory) {
			this.beanFactory = (ListableBeanFactory) beanFactory;
		}
	}


	@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		InjectionMetadata metadata = findPersistenceMetadata(beanName, beanType, null);
		metadata.checkConfigMembers(beanDefinition);
	}

	@Override
	public Object postProcessBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException {
		return null;
	}

	@Override
	public boolean postProcessAfterInstantiation(Object bean, String beanName) throws BeansException {
		return true;
	}

	//处理持久化相关属性
	@Override
	public PropertyValues postProcessPropertyValues(
			PropertyValues pvs, PropertyDescriptor[] pds, Object bean, String beanName) throws BeansException {

		//查找给定类中持久化元信息
		InjectionMetadata metadata = findPersistenceMetadata(beanName, bean.getClass(), pvs);
		try {
			//为Bean注入持久化属性
			metadata.inject(bean, beanName, pvs);
		}
		catch (Throwable ex) {
			throw new BeanCreationException(beanName, "Injection of persistence dependencies failed", ex);
		}
		return pvs;
	}

	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

	@Override
	public void postProcessBeforeDestruction(Object bean, String beanName) throws BeansException {
		EntityManager emToClose = this.extendedEntityManagersToClose.remove(bean);
		EntityManagerFactoryUtils.closeEntityManager(emToClose);
	}

	@Override
	public boolean requiresDestruction(Object bean) {
		return this.extendedEntityManagersToClose.containsKey(bean);
	}


	//查找给定类中的持久化元信息
	private InjectionMetadata findPersistenceMetadata(String beanName, final Class<?> clazz, @Nullable PropertyValues pvs) {
		// Fall back to class name as cache key, for backwards compatibility with custom callers.
		String cacheKey = (StringUtils.hasLength(beanName) ? beanName : clazz.getName());
		// Quick check on the concurrent map first, with minimal locking.
		//首先从容器缓存中查找给定类的元信息
		InjectionMetadata metadata = this.injectionMetadataCache.get(cacheKey);
		if (InjectionMetadata.needsRefresh(metadata, clazz)) {
			synchronized (this.injectionMetadataCache) {
				metadata = this.injectionMetadataCache.get(cacheKey);
				if (InjectionMetadata.needsRefresh(metadata, clazz)) {
					if (metadata != null) {
						metadata.clear(pvs);
					}
					metadata = buildPersistenceMetadata(clazz);
					this.injectionMetadataCache.put(cacheKey, metadata);
				}
			}
		}
		return metadata;
	}

	private InjectionMetadata buildPersistenceMetadata(final Class<?> clazz) {
		LinkedList<InjectionMetadata.InjectedElement> elements = new LinkedList<>();
		Class<?> targetClass = clazz;

		do {
			final LinkedList<InjectionMetadata.InjectedElement> currElements =
					new LinkedList<>();

			ReflectionUtils.doWithLocalFields(targetClass, field -> {
				if (field.isAnnotationPresent(PersistenceContext.class) ||
						field.isAnnotationPresent(PersistenceUnit.class)) {
					if (Modifier.isStatic(field.getModifiers())) {
						throw new IllegalStateException("Persistence annotations are not supported on static fields");
					}
					currElements.add(new PersistenceElement(field, field, null));
				}
			});

			ReflectionUtils.doWithLocalMethods(targetClass, method -> {
				Method bridgedMethod = BridgeMethodResolver.findBridgedMethod(method);
				if (!BridgeMethodResolver.isVisibilityBridgeMethodPair(method, bridgedMethod)) {
					return;
				}
				if ((bridgedMethod.isAnnotationPresent(PersistenceContext.class) ||
						bridgedMethod.isAnnotationPresent(PersistenceUnit.class)) &&
						method.equals(ClassUtils.getMostSpecificMethod(method, clazz))) {
					if (Modifier.isStatic(method.getModifiers())) {
						throw new IllegalStateException("Persistence annotations are not supported on static methods");
					}
					if (method.getParameterCount() != 1) {
						throw new IllegalStateException("Persistence annotation requires a single-arg method: " + method);
					}
					PropertyDescriptor pd = BeanUtils.findPropertyForMethod(bridgedMethod, clazz);
					currElements.add(new PersistenceElement(method, bridgedMethod, pd));
				}
			});

			elements.addAll(0, currElements);
			targetClass = targetClass.getSuperclass();
		}
		while (targetClass != null && targetClass != Object.class);

		return new InjectionMetadata(clazz, elements);
	}

	/**
	 * Return a specified persistence unit for the given unit name,
	 * as defined through the "persistenceUnits" map.
	 * @param unitName the name of the persistence unit
	 * @return the corresponding EntityManagerFactory,
	 * or {@code null} if none found
	 * @see #setPersistenceUnits
	 */
	//根据持久化单元名称获取实体管理器工厂
	@Nullable
	protected EntityManagerFactory getPersistenceUnit(@Nullable String unitName) {
		//如果持久化单元不为null
		if (this.persistenceUnits != null) {
			//获取持久化单元名称
			String unitNameForLookup = (unitName != null ? unitName : "");
			//如果持久化单元名称为空字符串
			if ("".equals(unitNameForLookup)) {
				//将默认的持久化单元名称作为持久化单元名称
				unitNameForLookup = this.defaultPersistenceUnitName;
			}
			//获取持久化单元的JNDI名称
			String jndiName = this.persistenceUnits.get(unitNameForLookup);
			if (jndiName == null && "".equals(unitNameForLookup) && this.persistenceUnits.size() == 1) {
				//从持久化单元中获取JNDI名称
				jndiName = this.persistenceUnits.values().iterator().next();
			}
			if (jndiName != null) {
				try {
					//查找指定JNDI名称的实体管理器工厂
					return lookup(jndiName, EntityManagerFactory.class);
				}
				catch (Exception ex) {
					throw new IllegalStateException("Could not obtain EntityManagerFactory [" + jndiName + "] from JNDI", ex);
				}
			}
		}
		return null;
	}

	/**
	 * Return a specified persistence context for the given unit name, as defined
	 * through the "persistenceContexts" (or "extendedPersistenceContexts") map.
	 * @param unitName the name of the persistence unit
	 * @param extended whether to obtain an extended persistence context
	 * @return the corresponding EntityManager, or {@code null} if none found
	 * @see #setPersistenceContexts
	 * @see #setExtendedPersistenceContexts
	 */
	//查找给定持久化单元名称的实体管理器
	@Nullable
	protected EntityManager getPersistenceContext(@Nullable String unitName, boolean extended) {
		//获取持久化上下文
		Map<String, String> contexts = (extended ? this.extendedPersistenceContexts : this.persistenceContexts);
		//持久化上下文不为null
		if (contexts != null) {
			String unitNameForLookup = (unitName != null ? unitName : "");
			if ("".equals(unitNameForLookup)) {
				unitNameForLookup = this.defaultPersistenceUnitName;
			}
			//从持久化上下文中获取给定持久化单元名称的JNDI
			String jndiName = contexts.get(unitNameForLookup);
			if (jndiName == null && "".equals(unitNameForLookup) && contexts.size() == 1) {
				jndiName = contexts.values().iterator().next();
			}
			if (jndiName != null) {
				try {
					//查找指定JNDI名称的实体管理器
					return lookup(jndiName, EntityManager.class);
				}
				catch (Exception ex) {
					throw new IllegalStateException("Could not obtain EntityManager [" + jndiName + "] from JNDI", ex);
				}
			}
		}
		return null;
	}

	/**
	 * Find an EntityManagerFactory with the given name in the current Spring
	 * application context, falling back to a single default EntityManagerFactory
	 * (if any) in case of no unit name specified.
	 * @param unitName the name of the persistence unit (may be {@code null} or empty)
	 * @param requestingBeanName the name of the requesting bean
	 * @return the EntityManagerFactory
	 * @throws NoSuchBeanDefinitionException if there is no such EntityManagerFactory in the context
	 */
	//从Spring容器中查找给定名称的实体管理器工厂
	protected EntityManagerFactory findEntityManagerFactory(@Nullable String unitName, @Nullable String requestingBeanName)
			throws NoSuchBeanDefinitionException {

		//获取持久化单元名称
		String unitNameForLookup = (unitName != null ? unitName : "");
		//如果持久化单元名称为空，则将容器默认持久化单元名称作用持久化单元名称
		if ("".equals(unitNameForLookup)) {
			unitNameForLookup = this.defaultPersistenceUnitName;
		}
		//如果持久化单元名称不为空，查找给定持久化单元名称的实体管理器工厂
		if (!"".equals(unitNameForLookup)) {
			return findNamedEntityManagerFactory(unitNameForLookup, requestingBeanName);
		}
		//如果持久化单元名称为空，获取容器中默认的实体管理器工厂
		else {
			return findDefaultEntityManagerFactory(requestingBeanName);
		}
	}

	/**
	 * Find an EntityManagerFactory with the given name in the current
	 * Spring application context.
	 * @param unitName the name of the persistence unit (never empty)
	 * @param requestingBeanName the name of the requesting bean
	 * @return the EntityManagerFactory
	 * @throws NoSuchBeanDefinitionException if there is no such EntityManagerFactory in the context
	 */
	//查找给定持久化单元名称的实体管理器工厂
	protected EntityManagerFactory findNamedEntityManagerFactory(String unitName, @Nullable String requestingBeanName)
			throws NoSuchBeanDefinitionException {

		Assert.state(this.beanFactory != null, "ListableBeanFactory required for EntityManagerFactory bean lookup");

		//从Spring所有容器中查找给定名称的实体管理器工厂对象
		EntityManagerFactory emf = EntityManagerFactoryUtils.findEntityManagerFactory(this.beanFactory, unitName);
		//将持久化单元名称和查找到的实体管理器工厂对象向Spring容器注册
		if (requestingBeanName != null && this.beanFactory instanceof ConfigurableBeanFactory) {
			((ConfigurableBeanFactory) this.beanFactory).registerDependentBean(unitName, requestingBeanName);
		}
		return emf;
	}

	/**
	 * Find a single default EntityManagerFactory in the Spring application context.
	 * @return the default EntityManagerFactory
	 * @throws NoSuchBeanDefinitionException if there is no single EntityManagerFactory in the context
	 */
	//获取容器默认的实体管理器工厂
	protected EntityManagerFactory findDefaultEntityManagerFactory(@Nullable String requestingBeanName)
			throws NoSuchBeanDefinitionException {

		Assert.state(this.beanFactory != null, "ListableBeanFactory required for EntityManagerFactory bean lookup");

		//将查找到的实体管理器工厂Bean和持久化单元名称向Spring容器注册
		if (this.beanFactory instanceof ConfigurableListableBeanFactory) {
			// Fancy variant with dependency registration
			ConfigurableListableBeanFactory clbf = (ConfigurableListableBeanFactory) this.beanFactory;
			NamedBeanHolder<EntityManagerFactory> emfHolder = clbf.resolveNamedBean(EntityManagerFactory.class);
			if (requestingBeanName != null) {
				clbf.registerDependentBean(emfHolder.getBeanName(), requestingBeanName);
			}
			return emfHolder.getBeanInstance();
		}
		else {
			// Plain variant: just find a default bean
			return this.beanFactory.getBean(EntityManagerFactory.class);
		}
	}

	/**
	 * Perform a JNDI lookup for the given resource by name.
	 * <p>Called for EntityManagerFactory and EntityManager lookup
	 * when JNDI names are mapped for specific persistence units.
	 * @param jndiName the JNDI name to look up
	 * @param requiredType the required type of the object
	 * @return the obtained object
	 * @throws Exception if the JNDI lookup failed
	 */
	protected <T> T lookup(String jndiName, Class<T> requiredType) throws Exception {
		return new LocatorDelegate().lookup(jndiName, requiredType);
	}


	/**
	 * Separate inner class to isolate the JNDI API dependency
	 * (for compatibility with Google App Engine's API white list).
	 */
	private class LocatorDelegate {

		public <T> T lookup(String jndiName, Class<T> requiredType) throws Exception {
			JndiLocatorDelegate locator = new JndiLocatorDelegate();
			if (jndiEnvironment instanceof JndiTemplate) {
				locator.setJndiTemplate((JndiTemplate) jndiEnvironment);
			}
			else if (jndiEnvironment instanceof Properties) {
				locator.setJndiEnvironment((Properties) jndiEnvironment);
			}
			else if (jndiEnvironment != null) {
				throw new IllegalStateException("Illegal 'jndiEnvironment' type: " + jndiEnvironment.getClass());
			}
			locator.setResourceRef(resourceRef);
			return locator.lookup(jndiName, requiredType);
		}
	}


	/**
	 * Class representing injection information about an annotated field
	 * or setter method.
	 */
	private class PersistenceElement extends InjectionMetadata.InjectedElement {

		private final String unitName;

		@Nullable
		private PersistenceContextType type;

		private boolean synchronizedWithTransaction = false;

		@Nullable
		private Properties properties;

		//解析持久化注解
		public PersistenceElement(Member member, AnnotatedElement ae, @Nullable PropertyDescriptor pd) {
			super(member, pd);
			//获取当前元素对象的@PersistenceContext注解
			PersistenceContext pc = ae.getAnnotation(PersistenceContext.class);
			//获取当前元素对象的@PersistenceUnit注解
			PersistenceUnit pu = ae.getAnnotation(PersistenceUnit.class);
			//资源类型为实体管理器
			Class<?> resourceType = EntityManager.class;
			//如果元素配置的持久化上下文不为null
			if (pc != null) {
				//如果元素配置的持久化单元也不为null
				if (pu != null) {
					//持久化上下文和持久化单元只能二选一，不能同时配置
					throw new IllegalStateException("Member may only be annotated with either " +
							"@PersistenceContext or @PersistenceUnit, not both: " + member);
				}
				Properties properties = null;
				//获取持久化上下文的属性
				PersistenceProperty[] pps = pc.properties();
				//如果持久化上下文属性不为空
				if (!ObjectUtils.isEmpty(pps)) {
					properties = new Properties();
					//将持久化上下文的属性名称和属性值保存到属性集合中
					for (PersistenceProperty pp : pps) {
						properties.setProperty(pp.name(), pp.value());
					}
				}
				//设置元素的持久化单元名称
				this.unitName = pc.unitName();
				//设置元素的持久化类型为实体管理器(EntityManager)
				this.type = pc.type();
				this.synchronizedWithTransaction = SynchronizationType.SYNCHRONIZED.equals(pc.synchronization());
				//设置元素的持久化属性
				this.properties = properties;
			}
			//如果持久化上下文为null
			else {
				//持久化资源类型为实体管理器工厂
				resourceType = EntityManagerFactory.class;
				this.unitName = pu.unitName();
			}
			//检查给定的持久化资源类型是字段还是方法类型
			checkResourceType(resourceType);
		}

		/**
		 * Resolve the object against the application context.
		 */
		//获取并注入持久化资源
		@Override
		protected Object getResourceToInject(Object target, @Nullable String requestingBeanName) {
			// Resolves to EntityManagerFactory or EntityManager.
			//持久化资源类型不为null，创建实体管理器
			if (this.type != null) {
				return (this.type == PersistenceContextType.EXTENDED ?
						resolveExtendedEntityManager(target, requestingBeanName) :
						resolveEntityManager(requestingBeanName));
			}
			//持久化资源类型为null，创建实体管理器工厂
			else {
				// OK, so we need an EntityManagerFactory...
				return resolveEntityManagerFactory(requestingBeanName);
			}
		}

		//创建实体管理器工厂
		private EntityManagerFactory resolveEntityManagerFactory(@Nullable String requestingBeanName) {
			// Obtain EntityManagerFactory from JNDI?
			//以持久化单元名称作为JNDI名称获取实体管理器工厂
			EntityManagerFactory emf = getPersistenceUnit(this.unitName);
			if (emf == null) {
				// Need to search for EntityManagerFactory beans.
				//根据持久化单元名称向Spring容器查找指定名称的实体管理器工厂
				emf = findEntityManagerFactory(this.unitName, requestingBeanName);
			}
			return emf;
		}

		//创建实体管理器
		private EntityManager resolveEntityManager(@Nullable String requestingBeanName) {
			// Obtain EntityManager reference from JNDI?
			//以持久化单元名称作为JNDI名称获取持久化上下文作为实体管理
			EntityManager em = getPersistenceContext(this.unitName, false);
			if (em == null) {
				// No pre-built EntityManager found -> build one based on factory.
				// Obtain EntityManagerFactory from JNDI?
				//以持久化单元名称作为JNDI名称获取实体管理器工厂
				EntityManagerFactory emf = getPersistenceUnit(this.unitName);
				//没有获取到指定JNDI名称的实体管理器工厂
				if (emf == null) {
					// Need to search for EntityManagerFactory beans.
					//根据持久化单元名称向Spring容器查找指定名称的实体管理器工厂
					emf = findEntityManagerFactory(this.unitName, requestingBeanName);
				}
				// Inject a shared transactional EntityManager proxy.
				//创建一个共享事务管理的实体管理器代理
				if (emf instanceof EntityManagerFactoryInfo &&
						((EntityManagerFactoryInfo) emf).getEntityManagerInterface() != null) {
					// Create EntityManager based on the info's vendor-specific type
					// (which might be more specific than the field's type).
					//根据JPA实现提供商类型创建相应的实体管理器
					em = SharedEntityManagerCreator.createSharedEntityManager(
							emf, this.properties, this.synchronizedWithTransaction);
				}
				else {
					// Create EntityManager based on the field's type.
					//根据持久化注解配置的持久化资源类型创建实体管理器
					em = SharedEntityManagerCreator.createSharedEntityManager(
							emf, this.properties, this.synchronizedWithTransaction, getResourceType());
				}
			}
			return em;
		}

		//创建扩展的实体管理器
		private EntityManager resolveExtendedEntityManager(Object target, @Nullable String requestingBeanName) {
			// Obtain EntityManager reference from JNDI?
			//以持久化单元名称作为JNDI名称获取持久化上下文作为实体管理器
			EntityManager em = getPersistenceContext(this.unitName, true);
			//如果根据JNDI获取到的持久化上下文为null
			if (em == null) {
				// No pre-built EntityManager found -> build one based on factory.
				// Obtain EntityManagerFactory from JNDI?
				//以持久化单元名称作为JNDI名称获取实体管理器工厂
				EntityManagerFactory emf = getPersistenceUnit(this.unitName);
				//没有获取到指定JNDI名称的实体管理器工厂
				if (emf == null) {
					// Need to search for EntityManagerFactory beans.
					//根据持久化单元名称向Spring容器查找指定名称的实体管理器工厂
					emf = findEntityManagerFactory(this.unitName, requestingBeanName);
				}
				// Inject a container-managed extended EntityManager.
				//根据实体管理器工厂和持久化属性创建一个容器管理的实体管理器
				em = ExtendedEntityManagerCreator.createContainerManagedEntityManager(
						emf, this.properties, this.synchronizedWithTransaction);
			}
			//创建实体管理器代理委派调用的实体管理器
			if (em instanceof EntityManagerProxy && beanFactory != null && requestingBeanName != null &&
					beanFactory.containsBean(requestingBeanName) && !beanFactory.isPrototype(requestingBeanName)) {
				extendedEntityManagersToClose.put(target, ((EntityManagerProxy) em).getTargetEntityManager());
			}
			return em;
		}
	}

}
