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

package org.springframework.beans.factory.annotation;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.TypeConverter;
import org.springframework.beans.factory.*;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.DependencyDescriptor;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessorAdapter;
import org.springframework.beans.factory.support.LookupOverride;
import org.springframework.beans.factory.support.MergedBeanDefinitionPostProcessor;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.core.BridgeMethodResolver;
import org.springframework.core.MethodParameter;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * {@link org.springframework.beans.factory.config.BeanPostProcessor} implementation
 * that autowires annotated fields, setter methods and arbitrary config methods.
 * Such members to be injected are detected through a Java 5 annotation: by default,
 * Spring's {@link Autowired @Autowired} and {@link Value @Value} annotations.
 *
 * <p>Also supports JSR-330's {@link javax.inject.Inject @Inject} annotation,
 * if available, as a direct alternative to Spring's own {@code @Autowired}.
 *
 * <p>Only one constructor (at max) of any given bean class may carry this
 * annotation with the 'required' parameter set to {@code true},
 * indicating <i>the</i> constructor to autowire when used as a Spring bean.
 * If multiple <i>non-required</i> constructors carry the annotation, they
 * will be considered as candidates for autowiring. The constructor with
 * the greatest number of dependencies that can be satisfied by matching
 * beans in the Spring container will be chosen. If none of the candidates
 * can be satisfied, then a default constructor (if present) will be used.
 * An annotated constructor does not have to be public.
 *
 * <p>Fields are injected right after construction of a bean, before any
 * config methods are invoked. Such a config field does not have to be public.
 *
 * <p>Config methods may have an arbitrary name and any number of arguments; each of
 * those arguments will be autowired with a matching bean in the Spring container.
 * Bean property setter methods are effectively just a special case of such a
 * general config method. Config methods do not have to be public.
 *
 * <p>Note: A default AutowiredAnnotationBeanPostProcessor will be registered
 * by the "context:annotation-config" and "context:component-scan" XML tags.
 * Remove or turn off the default annotation configuration there if you intend
 * to specify a custom AutowiredAnnotationBeanPostProcessor bean definition.
 * <p><b>NOTE:</b> Annotation injection will be performed <i>before</i> XML injection;
 * thus the latter configuration will override the former for properties wired through
 * both approaches.
 *
 * <p>In addition to regular injection points as discussed above, this post-processor
 * also handles Spring's {@link Lookup @Lookup} annotation which identifies lookup
 * methods to be replaced by the container at runtime. This is essentially a type-safe
 * version of {@code getBean(Class, args)} and {@code getBean(String, args)},
 * See {@link Lookup @Lookup's javadoc} for details.
 *
 * @author Juergen Hoeller
 * @author Mark Fisher
 * @author Stephane Nicoll
 * @author Sebastien Deleuze
 * @since 2.5
 * @see #setAutowiredAnnotationType
 * @see Autowired
 * @see Value
 */
public class AutowiredAnnotationBeanPostProcessor extends InstantiationAwareBeanPostProcessorAdapter
		implements MergedBeanDefinitionPostProcessor, PriorityOrdered, BeanFactoryAware {

	protected final Log logger = LogFactory.getLog(getClass());

	private final Set<Class<? extends Annotation>> autowiredAnnotationTypes = new LinkedHashSet<>();

	private String requiredParameterName = "required";

	private boolean requiredParameterValue = true;

	private int order = Ordered.LOWEST_PRECEDENCE - 2;

	@Nullable
	private ConfigurableListableBeanFactory beanFactory;

	private final Set<String> lookupMethodsChecked = Collections.newSetFromMap(new ConcurrentHashMap<>(256));

	private final Map<Class<?>, Constructor<?>[]> candidateConstructorsCache = new ConcurrentHashMap<>(256);

	private final Map<String, InjectionMetadata> injectionMetadataCache = new ConcurrentHashMap<>(256);


	/**
	 * Create a new AutowiredAnnotationBeanPostProcessor
	 * for Spring's standard {@link Autowired} annotation.
	 * <p>Also supports JSR-330's {@link javax.inject.Inject} annotation, if available.
	 */
	@SuppressWarnings("unchecked")
	public AutowiredAnnotationBeanPostProcessor() {
		//后置处理器将处理@Autowire注解
		this.autowiredAnnotationTypes.add(Autowired.class);
		//后置处理器将处理@Value注解
		this.autowiredAnnotationTypes.add(Value.class);
		try {
			//后置处理器将处理javax.inject.Inject JSR-330注解
			this.autowiredAnnotationTypes.add((Class<? extends Annotation>)
					ClassUtils.forName("javax.inject.Inject", AutowiredAnnotationBeanPostProcessor.class.getClassLoader()));
			logger.info("JSR-330 'javax.inject.Inject' annotation found and supported for autowiring");
		}
		catch (ClassNotFoundException ex) {
			// JSR-330 API not available - simply skip.
		}
	}


	/**
	 * Set the 'autowired' annotation type, to be used on constructors, fields,
	 * setter methods and arbitrary config methods.
	 * <p>The default autowired annotation type is the Spring-provided
	 * {@link Autowired} annotation, as well as {@link Value}.
	 * <p>This setter property exists so that developers can provide their own
	 * (non-Spring-specific) annotation type to indicate that a member is
	 * supposed to be autowired.
	 */
	public void setAutowiredAnnotationType(Class<? extends Annotation> autowiredAnnotationType) {
		Assert.notNull(autowiredAnnotationType, "'autowiredAnnotationType' must not be null");
		this.autowiredAnnotationTypes.clear();
		this.autowiredAnnotationTypes.add(autowiredAnnotationType);
	}

	/**
	 * Set the 'autowired' annotation types, to be used on constructors, fields,
	 * setter methods and arbitrary config methods.
	 * <p>The default autowired annotation type is the Spring-provided
	 * {@link Autowired} annotation, as well as {@link Value}.
	 * <p>This setter property exists so that developers can provide their own
	 * (non-Spring-specific) annotation types to indicate that a member is
	 * supposed to be autowired.
	 */
	public void setAutowiredAnnotationTypes(Set<Class<? extends Annotation>> autowiredAnnotationTypes) {
		Assert.notEmpty(autowiredAnnotationTypes, "'autowiredAnnotationTypes' must not be empty");
		this.autowiredAnnotationTypes.clear();
		this.autowiredAnnotationTypes.addAll(autowiredAnnotationTypes);
	}

	/**
	 * Set the name of a parameter of the annotation that specifies
	 * whether it is required.
	 * @see #setRequiredParameterValue(boolean)
	 */
	public void setRequiredParameterName(String requiredParameterName) {
		this.requiredParameterName = requiredParameterName;
	}

	/**
	 * Set the boolean value that marks a dependency as required
	 * <p>For example if using 'required=true' (the default),
	 * this value should be {@code true}; but if using
	 * 'optional=false', this value should be {@code false}.
	 * @see #setRequiredParameterName(String)
	 */
	public void setRequiredParameterValue(boolean requiredParameterValue) {
		this.requiredParameterValue = requiredParameterValue;
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
		if (!(beanFactory instanceof ConfigurableListableBeanFactory)) {
			throw new IllegalArgumentException(
					"AutowiredAnnotationBeanPostProcessor requires a ConfigurableListableBeanFactory: " + beanFactory);
		}
		this.beanFactory = (ConfigurableListableBeanFactory) beanFactory;
	}


	@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		InjectionMetadata metadata = findAutowiringMetadata(beanName, beanType, null);
		metadata.checkConfigMembers(beanDefinition);
	}

	//为自动依赖注入装配Bean选择合适的构造方法
	@Override
	@Nullable
	public Constructor<?>[] determineCandidateConstructors(Class<?> beanClass, final String beanName)
			throws BeanCreationException {

		// Let's check for lookup methods here..
		if (!this.lookupMethodsChecked.contains(beanName)) {
			try {
				ReflectionUtils.doWithMethods(beanClass, method -> {
					Lookup lookup = method.getAnnotation(Lookup.class);
					if (lookup != null) {
						Assert.state(beanFactory != null, "No BeanFactory available");
						LookupOverride override = new LookupOverride(method, lookup.value());
						try {
							RootBeanDefinition mbd = (RootBeanDefinition) beanFactory.getMergedBeanDefinition(beanName);
							mbd.getMethodOverrides().addOverride(override);
						}
						catch (NoSuchBeanDefinitionException ex) {
							throw new BeanCreationException(beanName,
								"Cannot apply @Lookup to beans without corresponding bean definition");
						}
					}
				});
			}
			catch (IllegalStateException ex) {
				throw new BeanCreationException(beanName, "Lookup method resolution failed", ex);
			}
			this.lookupMethodsChecked.add(beanName);
		}

		//首先从容器的缓存中查找是否有指定Bean的构造方法
		// Quick check on the concurrent map first, with minimal locking.
		Constructor<?>[] candidateConstructors = this.candidateConstructorsCache.get(beanClass);
		if (candidateConstructors == null) {
			// Fully synchronized resolution now...
			//线程同步以确保容器中数据一致性
			synchronized (this.candidateConstructorsCache) {
				candidateConstructors = this.candidateConstructorsCache.get(beanClass);
				if (candidateConstructors == null) {
					Constructor<?>[] rawCandidates;
					try {
						//通过JDK反射机制，获取指定类的中所有声明的构造方法
						rawCandidates = beanClass.getDeclaredConstructors();
					}
					catch (Throwable ex) {
						throw new BeanCreationException(beanName,
								"Resolution of declared constructors on bean Class [" + beanClass.getName() +
								"] from ClassLoader [" + beanClass.getClassLoader() + "] failed", ex);
					}
					//存放候选构造方法的集合
					List<Constructor<?>> candidates = new ArrayList<Constructor<?>>(rawCandidates.length);
					//autowire注解中required属性指定的构造方法
					Constructor<?> requiredConstructor = null;
					//默认的构造方法
					Constructor<?> defaultConstructor = null;
					//主构造方法
					Constructor<?> primaryConstructor = BeanUtils.findPrimaryConstructor(beanClass);
					int nonSyntheticConstructors = 0;
					//遍历所有的构造方法，检查是否添加了autowire注解，以及是否指定了required属性
					for (Constructor<?> candidate : rawCandidates) {
						if (!candidate.isSynthetic()) {
							nonSyntheticConstructors++;
						}
						else if (primaryConstructor != null) {
							continue;
						}
						//获取指定类中所有关于autowire的注解(Annotation)
						AnnotationAttributes ann = findAutowiredAnnotation(candidate);
						//如果指定类中没有antowire的注解
						if (ann == null) {
							Class<?> userClass = ClassUtils.getUserClass(beanClass);
							if (userClass != beanClass) {
								try {
									Constructor<?> superCtor =
											userClass.getDeclaredConstructor(candidate.getParameterTypes());
									ann = findAutowiredAnnotation(superCtor);
								}
								catch (NoSuchMethodException ex) {
									// Simply proceed, no equivalent superclass constructor found...
								}
							}
						}
						//如果指定类中有关于antowire的注解
						if (ann != null) {
							//如果antowire注解中指定了required属性
							if (requiredConstructor != null) {
								throw new BeanCreationException(beanName,
										"Invalid autowire-marked constructor: " + candidate +
										". Found constructor with 'required' Autowired annotation already: " +
										requiredConstructor);
							}
							//获取autowire注解中required属性值
							boolean required = determineRequiredStatus(ann);
							//如果获取到autowire注解中required的属性值
							if (required) {
								//如果候选构造方法集合不为空
								if (!candidates.isEmpty()) {
									throw new BeanCreationException(beanName,
											"Invalid autowire-marked constructors: " + candidates +
											". Found constructor with 'required' Autowired annotation: " +
											candidate);
								}
								//当前的构造方法就是required属性所配置的构造方法
								requiredConstructor = candidate;
							}
							//将当前的构造方法添加到哦啊候选构造方法集合中
							candidates.add(candidate);
						}
						//如果autowire注解的参数列表为空
						else if (candidate.getParameterCount() == 0) {
							defaultConstructor = candidate;
						}
					}
					//如果候选构造方法集合不为空
					if (!candidates.isEmpty()) {
						// Add default constructor to list of optional constructors, as fallback.
						//如果所有的构造方法都没有配置required属性，且有默认构造方法
						if (requiredConstructor == null) {
							if (defaultConstructor != null) {
								//将默认构造方法添加到候选构造方法列表
								candidates.add(defaultConstructor);
							}
							else if (candidates.size() == 1 && logger.isWarnEnabled()) {
								logger.warn("Inconsistent constructor declaration on bean with name '" + beanName +
										"': single autowire-marked constructor flagged as optional - " +
										"this constructor is effectively required since there is no " +
										"default constructor to fall back to: " + candidates.get(0));
							}
						}
						//将候选构造方法集合转换为数组
						candidateConstructors = candidates.toArray(new Constructor<?>[candidates.size()]);
					}
					else if (rawCandidates.length == 1 && rawCandidates[0].getParameterCount() > 0) {
						candidateConstructors = new Constructor<?>[] {rawCandidates[0]};
					}
					else if (nonSyntheticConstructors == 2 && primaryConstructor != null && defaultConstructor != null) {
						candidateConstructors = new Constructor<?>[] {primaryConstructor, defaultConstructor};
					}
					else if (nonSyntheticConstructors == 1 && primaryConstructor != null) {
						candidateConstructors = new Constructor<?>[] {primaryConstructor};
					}
					else {
						//如果候选构造方法集合为空，则创建一个空的数组
						candidateConstructors = new Constructor<?>[0];
					}
					//将类的候选构造方法集合存放到容器的缓存中
					this.candidateConstructorsCache.put(beanClass, candidateConstructors);
				}
			}
		}
		//返回指定类的候选构造方法数组，如果没有返回null
		return (candidateConstructors.length > 0 ? candidateConstructors : null);
	}

	//处理类中的属性
	@Override
	public PropertyValues postProcessPropertyValues(
			PropertyValues pvs, PropertyDescriptor[] pds, Object bean, String beanName) throws BeanCreationException {

		//获取指定类中autowire相关注解的元信息
		InjectionMetadata metadata = findAutowiringMetadata(beanName, bean.getClass(), pvs);
		try {
			//对Bean的属性进行自动注入
			metadata.inject(bean, beanName, pvs);
		}
		catch (BeanCreationException ex) {
			throw ex;
		}
		catch (Throwable ex) {
			throw new BeanCreationException(beanName, "Injection of autowired dependencies failed", ex);
		}
		return pvs;
	}

	/**
	 * 'Native' processing method for direct calls with an arbitrary target instance,
	 * resolving all of its fields and methods which are annotated with {@code @Autowired}.
	 * @param bean the target instance to process
	 * @throws BeanCreationException if autowiring failed
	 */
	//处理对象的注入
	public void processInjection(Object bean) throws BeanCreationException {
		//获取给定Bean的Class对象
		Class<?> clazz = bean.getClass();
		//获取给定类中autowire相关注解元信息
		InjectionMetadata metadata = findAutowiringMetadata(clazz.getName(), clazz, null);
		try {
			//对Bean对象进行自动注入
			metadata.inject(bean, null, null);
		}
		catch (BeanCreationException ex) {
			throw ex;
		}
		catch (Throwable ex) {
			throw new BeanCreationException(
					"Injection of autowired dependencies failed for class [" + clazz + "]", ex);
		}
	}


	//获取给定类的autowire相关注解元信息
	private InjectionMetadata findAutowiringMetadata(String beanName, Class<?> clazz, @Nullable PropertyValues pvs) {
		// Fall back to class name as cache key, for backwards compatibility with custom callers.
		String cacheKey = (StringUtils.hasLength(beanName) ? beanName : clazz.getName());
		// Quick check on the concurrent map first, with minimal locking.
		//首先从容器中查找是否有给定类的autowire相关注解元信息
		InjectionMetadata metadata = this.injectionMetadataCache.get(cacheKey);
		if (InjectionMetadata.needsRefresh(metadata, clazz)) {
			synchronized (this.injectionMetadataCache) {
				metadata = this.injectionMetadataCache.get(cacheKey);
				if (InjectionMetadata.needsRefresh(metadata, clazz)) {
					if (metadata != null) {
						//解析给定类autowire相关注解元信息
						metadata.clear(pvs);
					}
					//解析给定类autowire相关注解元信息
					metadata = buildAutowiringMetadata(clazz);
					//将得到的给定类autowire相关注解元信息存储在容器缓存中
					this.injectionMetadataCache.put(cacheKey, metadata);
				}
			}
		}
		return metadata;
	}

	//解析给定类autowire相关注解元信息
	private InjectionMetadata buildAutowiringMetadata(final Class<?> clazz) {
		//创建一个存放注解元信息的集合
		LinkedList<InjectionMetadata.InjectedElement> elements = new LinkedList<>();
		Class<?> targetClass = clazz;

		//递归遍历当前类及其所有基类，解析全部注解元信息
		do {
			//创建一个存储当前正在处理类注解元信息的集合
			final LinkedList<InjectionMetadata.InjectedElement> currElements = new LinkedList<>();

			//利用JDK反射机制获取给定类中所有的声明字段，获取字段上的注解信息
			ReflectionUtils.doWithLocalFields(targetClass, field -> {
				//获取给定字段上的注解
				AnnotationAttributes ann = findAutowiredAnnotation(field);
				if (ann != null) {
					//如果给定字段是静态的(static)，则直接遍历下一个字段
					if (Modifier.isStatic(field.getModifiers())) {
						if (logger.isWarnEnabled()) {
							logger.warn("Autowired annotation is not supported on static fields: " + field);
						}
						return;
					}
					//判断注解的required属性值是否有效
					boolean required = determineRequiredStatus(ann);
					//将当前字段元信息封装，添加在返回的集合中
					currElements.add(new AutowiredFieldElement(field, required));
				}
			});

			//利用JDK反射机制获取给定类中所有的声明方法，获取方法上的注解信息
			ReflectionUtils.doWithLocalMethods(targetClass, method -> {
				Method bridgedMethod = BridgeMethodResolver.findBridgedMethod(method);
				if (!BridgeMethodResolver.isVisibilityBridgeMethodPair(method, bridgedMethod)) {
					return;
				}
				//获取给定方法上的所有注解
				AnnotationAttributes ann = findAutowiredAnnotation(bridgedMethod);
				if (ann != null && method.equals(ClassUtils.getMostSpecificMethod(method, clazz))) {
					//如果方法是静态的，则直接遍历下一个方法
					if (Modifier.isStatic(method.getModifiers())) {
						if (logger.isWarnEnabled()) {
							logger.warn("Autowired annotation is not supported on static methods: " + method);
						}
						return;
					}
					//如果方法的参数列表为空
					if (method.getParameterCount() == 0) {
						if (logger.isWarnEnabled()) {
							logger.warn("Autowired annotation should only be used on methods with parameters: " +
									method);
						}
					}
					//判断注解的required属性值是否有效
					boolean required = determineRequiredStatus(ann);
					//获取当前方法的属性描述符，即方法是可读的(readable)getter方法，还是可写的(writeable)setter方法
					PropertyDescriptor pd = BeanUtils.findPropertyForMethod(bridgedMethod, clazz);
					//将方法元信息封装添加到返回的元信息集合中
					currElements.add(new AutowiredMethodElement(method, required, pd));
				}
			});

			//将当前类的注解元信息存放到注解元信息集合中
			elements.addAll(0, currElements);
			//获取给定类的父类
			targetClass = targetClass.getSuperclass();
		}
		//如果给定类有基类，并且基类不是Object，则递归获取其基类的元信息
		while (targetClass != null && targetClass != Object.class);

		return new InjectionMetadata(clazz, elements);
	}

	//获取给定对象的autowire相关注解
	@Nullable
	private AnnotationAttributes findAutowiredAnnotation(AccessibleObject ao) {
		if (ao.getAnnotations().length > 0) {
			//遍历所有autowire相关的注解:@Autowire、@Value以及JSR-330等
			for (Class<? extends Annotation> type : this.autowiredAnnotationTypes) {
				//获取给定对象上的指定类型的注解
				AnnotationAttributes attributes = AnnotatedElementUtils.getMergedAnnotationAttributes(ao, type);
				if (attributes != null) {
					return attributes;
				}
			}
		}
		return null;
	}

	/**
	 * Determine if the annotated field or method requires its dependency.
	 * <p>A 'required' dependency means that autowiring should fail when no beans
	 * are found. Otherwise, the autowiring process will simply bypass the field
	 * or method when no beans are found.
	 * @param ann the Autowired annotation
	 * @return whether the annotation indicates that a dependency is required
	 */
	protected boolean determineRequiredStatus(AnnotationAttributes ann) {
		return (!ann.containsKey(this.requiredParameterName) ||
				this.requiredParameterValue == ann.getBoolean(this.requiredParameterName));
	}

	/**
	 * Obtain all beans of the given type as autowire candidates.
	 * @param type the type of the bean
	 * @return the target beans, or an empty Collection if no bean of this type is found
	 * @throws BeansException if bean retrieval failed
	 */
	protected <T> Map<String, T> findAutowireCandidates(Class<T> type) throws BeansException {
		if (this.beanFactory == null) {
			throw new IllegalStateException("No BeanFactory configured - " +
					"override the getBeanOfType method or specify the 'beanFactory' property");
		}
		return BeanFactoryUtils.beansOfTypeIncludingAncestors(this.beanFactory, type);
	}

	/**
	 * Register the specified bean as dependent on the autowired beans.
	 */
	private void registerDependentBeans(@Nullable String beanName, Set<String> autowiredBeanNames) {
		if (beanName != null) {
			for (String autowiredBeanName : autowiredBeanNames) {
				if (this.beanFactory != null && this.beanFactory.containsBean(autowiredBeanName)) {
					this.beanFactory.registerDependentBean(autowiredBeanName, beanName);
				}
				if (logger.isDebugEnabled()) {
					logger.debug("Autowiring by type from bean name '" + beanName +
							"' to bean named '" + autowiredBeanName + "'");
				}
			}
		}
	}

	/**
	 * Resolve the specified cached method argument or field value.
	 */
	@Nullable
	private Object resolvedCachedArgument(@Nullable String beanName, @Nullable Object cachedArgument) {
		if (cachedArgument instanceof DependencyDescriptor) {
			DependencyDescriptor descriptor = (DependencyDescriptor) cachedArgument;
			Assert.state(beanFactory != null, "No BeanFactory available");
			return this.beanFactory.resolveDependency(descriptor, beanName, null, null);
		}
		else {
			return cachedArgument;
		}
	}


	/**
	 * Class representing injection information about an annotated field.
	 */
	private class AutowiredFieldElement extends InjectionMetadata.InjectedElement {

		private final boolean required;

		private volatile boolean cached = false;

		@Nullable
		private volatile Object cachedFieldValue;

		public AutowiredFieldElement(Field field, boolean required) {
			super(field, null);
			this.required = required;
		}

		//对字段进行注入
		@Override
		protected void inject(Object bean, @Nullable String beanName, @Nullable PropertyValues pvs) throws Throwable {
			//获取注入元素对象
			Field field = (Field) this.member;
			Object value;
			//如果当前对象在容器中被缓存
			if (this.cached) {
				//根据Bean名称解析缓存中的字段值
				value = resolvedCachedArgument(beanName, this.cachedFieldValue);
			}
			//如果当前对象没有被容器缓存
			else {
				//创建一个字段依赖描述符
				DependencyDescriptor desc = new DependencyDescriptor(field, this.required);
				desc.setContainingClass(bean.getClass());
				Set<String> autowiredBeanNames = new LinkedHashSet<>(1);
				Assert.state(beanFactory != null, "No BeanFactory available");
				//获取容器中的类型转换器
				TypeConverter typeConverter = beanFactory.getTypeConverter();
				try {
					//根据容器中Bean定义，解析指定的依赖关系，获取依赖对象
					value = beanFactory.resolveDependency(desc, beanName, autowiredBeanNames, typeConverter);
				}
				catch (BeansException ex) {
					throw new UnsatisfiedDependencyException(null, beanName, new InjectionPoint(field), ex);
				}
				//线程同步，确保容器中数据一致性
				synchronized (this) {
					//如果当前对象没有被容器缓存
					if (!this.cached) {
						//获取到了当前对象的依赖对象，并且required属性为true
						if (value != null || this.required) {
							this.cachedFieldValue = desc;
							//为指定Bean注册依赖Bean
							registerDependentBeans(beanName, autowiredBeanNames);
							if (autowiredBeanNames.size() == 1) {
								String autowiredBeanName = autowiredBeanNames.iterator().next();
								//如果容器中有指定名称的Bean对象
								if (beanFactory.containsBean(autowiredBeanName)) {
									//依赖对象类型和字段类型匹配，默认按类型注入
									if (beanFactory.isTypeMatch(autowiredBeanName, field.getType())) {
										//创建一个依赖对象的引用，同时缓存
										this.cachedFieldValue = new ShortcutDependencyDescriptor(
												desc, autowiredBeanName, field.getType());
									}
								}
							}
						}
						//如果获取的依赖关系为null，且获取required属性为false
						else {
							//将字段值的缓存设置为null
							this.cachedFieldValue = null;
						}
						//容器已经对当前字段的值缓存
						this.cached = true;
					}
				}
			}
			//如果字段依赖值不为null
			if (value != null) {
				//显式使用JDK的反射机制，设置自动的访问控制权限为允许访问
				ReflectionUtils.makeAccessible(field);
				//为Bean对象的字段设置值
				field.set(bean, value);
			}
		}
	}


	/**
	 * Class representing injection information about an annotated method.
	 */
	private class AutowiredMethodElement extends InjectionMetadata.InjectedElement {

		private final boolean required;

		private volatile boolean cached = false;

		@Nullable
		private volatile Object[] cachedMethodArguments;

		public AutowiredMethodElement(Method method, boolean required, @Nullable PropertyDescriptor pd) {
			super(method, pd);
			this.required = required;
		}

		//对方法进行注入
		@Override
		protected void inject(Object bean, @Nullable String beanName, @Nullable PropertyValues pvs) throws Throwable {
			//如果属性被显式设置为skip，则不进行注入
			if (checkPropertySkipping(pvs)) {
				return;
			}
			//获取注入元素对象
			Method method = (Method) this.member;
			Object[] arguments;
			//如果容器对当前方法缓存
			if (this.cached) {
				// Shortcut for avoiding synchronization...
				//获取缓存中指定Bean名称的方法参数
				arguments = resolveCachedArguments(beanName);
			}
			//如果没有缓存
			else {
				//获取方法的参数列表
				Class<?>[] paramTypes = method.getParameterTypes();
				//创建一个存放方法参数的数组
				arguments = new Object[paramTypes.length];
				DependencyDescriptor[] descriptors = new DependencyDescriptor[paramTypes.length];
				Set<String> autowiredBeans = new LinkedHashSet<>(paramTypes.length);
				Assert.state(beanFactory != null, "No BeanFactory available");
				//获取容器的类型转换器
				TypeConverter typeConverter = beanFactory.getTypeConverter();
				for (int i = 0; i < arguments.length; i++) {
					//创建方法参数对象
					MethodParameter methodParam = new MethodParameter(method, i);
					DependencyDescriptor currDesc = new DependencyDescriptor(methodParam, this.required);
					currDesc.setContainingClass(bean.getClass());
					//解析方法的输入参数，为方法参数创建依赖描述符
					descriptors[i] = currDesc;
					try {
						Object arg = beanFactory.resolveDependency(currDesc, beanName, autowiredBeans, typeConverter);
						if (arg == null && !this.required) {
							arguments = null;
							break;
						}
						//根据容器中Bean定义解析依赖关系，获取方法参数依赖对象
						arguments[i] = arg;
					}
					catch (BeansException ex) {
						throw new UnsatisfiedDependencyException(null, beanName, new InjectionPoint(methodParam), ex);
					}
				}
				//线程同步，以确保容器中数据一致性
				synchronized (this) {
					//如果当前方法没有被容器缓存
					if (!this.cached) {
						//如果方法的参数列表不为空
						if (arguments != null) {
							//为容器中缓存方法参数的对象赋值
							Object[] cachedMethodArguments = new Object[paramTypes.length];
							for (int i = 0; i < arguments.length; i++) {
								cachedMethodArguments[i] = descriptors[i];
							}
							//为指定Bean注册依赖Bean
							registerDependentBeans(beanName, autowiredBeans);
							//如果依赖对象集合大小等于方法参数个数
							if (autowiredBeans.size() == paramTypes.length) {
								Iterator<String> it = autowiredBeans.iterator();
								//为方法参数设置依赖对象
								for (int i = 0; i < paramTypes.length; i++) {
									String autowiredBeanName = it.next();
									//如果容器中存在指定名称的Bean对象
									if (beanFactory.containsBean(autowiredBeanName)) {
										//如果参数类型和依赖对象类型匹配
										if (beanFactory.isTypeMatch(autowiredBeanName, paramTypes[i])) {
											//创建一个依赖对象的引用，复制给方法相应的参
											cachedMethodArguments[i] = new ShortcutDependencyDescriptor(
													descriptors[i], autowiredBeanName, paramTypes[i]);
										}
									}
								}
							}
							this.cachedMethodArguments = cachedMethodArguments;
						}
						//如果方法参数列表为null，则设置容器对该方法参数的缓存为null
						else {
							this.cachedMethodArguments = null;
						}
						//设置容器已经对该方法缓存
						this.cached = true;
					}
				}
			}
			//如果方法参数依赖对象不为null
			if (arguments != null) {
				try {
					//使用JDK的反射机制，显式设置方法的访问控制权限为允许访问
					ReflectionUtils.makeAccessible(method);
					//调用Bean的指定方法
					method.invoke(bean, arguments);
				}
				catch (InvocationTargetException ex){
					throw ex.getTargetException();
				}
			}
		}

		@Nullable
		private Object[] resolveCachedArguments(@Nullable String beanName) {
			Object[] cachedMethodArguments = this.cachedMethodArguments;
			if (cachedMethodArguments == null) {
				return null;
			}
			Object[] arguments = new Object[cachedMethodArguments.length];
			for (int i = 0; i < arguments.length; i++) {
				arguments[i] = resolvedCachedArgument(beanName, cachedMethodArguments[i]);
			}
			return arguments;
		}
	}


	/**
	 * DependencyDescriptor variant with a pre-resolved target bean name.
	 */
	@SuppressWarnings("serial")
	private static class ShortcutDependencyDescriptor extends DependencyDescriptor {

		private final String shortcut;

		private final Class<?> requiredType;

		public ShortcutDependencyDescriptor(DependencyDescriptor original, String shortcut, Class<?> requiredType) {
			super(original);
			this.shortcut = shortcut;
			this.requiredType = requiredType;
		}

		@Override
		public Object resolveShortcut(BeanFactory beanFactory) {
			return beanFactory.getBean(this.shortcut, this.requiredType);
		}
	}

}
