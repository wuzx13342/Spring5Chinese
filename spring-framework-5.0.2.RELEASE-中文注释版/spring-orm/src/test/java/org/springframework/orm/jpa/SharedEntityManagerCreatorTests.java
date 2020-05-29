/*
 * Copyright 2002-2014 the original author or authors.
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

package org.springframework.orm.jpa;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.TransactionRequiredException;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for {@link SharedEntityManagerCreator}.
 *
 * @author Oliver Gierke
 */
public class SharedEntityManagerCreatorTests {

	@Test
	public void proxyingWorksIfInfoReturnsNullEntityManagerInterface() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class,
				withSettings().extraInterfaces(EntityManagerFactoryInfo.class));
		// EntityManagerFactoryInfo.getEntityManagerInterface returns null
		assertThat(SharedEntityManagerCreator.createSharedEntityManager(emf), is(notNullValue()));
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnJoinTransaction() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.joinTransaction();
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnFlush() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.flush();
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnPersist() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.persist(new Object());
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnMerge() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.merge(new Object());
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnRemove() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.remove(new Object());
	}

	@Test(expected = TransactionRequiredException.class)
	public void transactionRequiredExceptionOnRefresh() {
		EntityManagerFactory emf = mock(EntityManagerFactory.class);
		EntityManager em = SharedEntityManagerCreator.createSharedEntityManager(emf);
		em.refresh(new Object());
	}

}
