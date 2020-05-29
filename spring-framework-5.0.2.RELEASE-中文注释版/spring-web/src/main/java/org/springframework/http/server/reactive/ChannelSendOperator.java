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

package org.springframework.http.server.reactive;

import java.util.function.Function;

import org.reactivestreams.Publisher;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;
import reactor.core.CoreSubscriber;
import reactor.core.Scannable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Operators;
import reactor.util.context.Context;

import org.springframework.lang.Nullable;
import org.springframework.util.Assert;

/**
 * Given a write function that accepts a source {@code Publisher<T>} to write
 * with and returns {@code Publisher<Void>} for the result, this operator helps
 * to defer the invocation of the write function, until we know if the source
 * publisher will begin publishing without an error. If the first emission is
 * an error, the write function is bypassed, and the error is sent directly
 * through the result publisher. Otherwise the write function is invoked.
 *
 * @author Rossen Stoyanchev
 * @author Stephane Maldini
 * @since 5.0
 */
public class ChannelSendOperator<T> extends Mono<Void> implements Scannable {

	private final Function<Publisher<T>, Publisher<Void>> writeFunction;

	private final Flux<T> source;


	public ChannelSendOperator(Publisher<? extends T> source, Function<Publisher<T>,
			Publisher<Void>> writeFunction) {

		this.source = Flux.from(source);
		this.writeFunction = writeFunction;
	}


	@Override
	@Nullable
	@SuppressWarnings("rawtypes")
	public Object scanUnsafe(Attr key) {
		if (key == Attr.PREFETCH) {
			return Integer.MAX_VALUE;
		}
		if (key == Attr.PARENT) {
			return this.source;
		}
		return null;
	}

	@Override
	public void subscribe(CoreSubscriber<? super Void> actual) {
		this.source.subscribe(new WriteBarrier(actual));
	}


	/**
	 * A barrier inserted between the write source and the write subscriber
	 * (i.e. the HTTP server adapter) that pre-fetches and waits for the first
	 * signal before deciding whether to hook in to the write subscriber.
	 *
	 * <p>Acts as:
	 * <ul>
	 * <li>Subscriber to the write source.
	 * <li>Subscription to the write subscriber.
	 * <li>Publisher to the write subscriber.
	 * </ul>
	 *
	 * <p>Also uses {@link WriteCompletionBarrier} to communicate completion
	 * and detect cancel signals from the completion subscriber.
	 */
	@SuppressWarnings("deprecation")
	private final class WriteBarrier implements CoreSubscriber<T>, Subscription, Publisher<T> {

		/* Bridges signals to and from the completionSubscriber */
		private final WriteCompletionBarrier writeCompletionBarrier;

		/* Upstream write source subscription */
		@Nullable
		private Subscription subscription;

		/**
		 * We've at at least one emission, we've called the write function, the write
		 * subscriber has subscribed and cached signals have been emitted to it.
		 * We're now simply passing data through to the write subscriber.
		 **/
		private boolean readyToWrite = false;

		/** No emission from upstream yet */
		private boolean beforeFirstEmission = true;

		/** Cached signal before readyToWrite */
		@Nullable
		private T item;

		/** Cached 1st/2nd signal before readyToWrite */
		@Nullable
		private Throwable error;

		/** Cached 1st/2nd signal before readyToWrite */
		private boolean completed = false;

		/** The actual writeSubscriber from the HTTP server adapter */
		@Nullable
		private Subscriber<? super T> writeSubscriber;


		WriteBarrier(CoreSubscriber<? super Void> completionSubscriber) {
			this.writeCompletionBarrier = new WriteCompletionBarrier(completionSubscriber, this);
		}


		// Subscriber<T> methods (we're the subscriber to the write source)..

		@Override
		public final void onSubscribe(Subscription s) {
			if (Operators.validate(this.subscription, s)) {
				this.subscription = s;
				this.writeCompletionBarrier.connect();
				s.request(1);
			}
		}

		@Override
		public final void onNext(T item) {
			if (this.readyToWrite) {
				requiredWriteSubscriber().onNext(item);
				return;
			}
			//FIXME revisit in case of reentrant sync deadlock
			synchronized (this) {
				if (this.readyToWrite) {
					requiredWriteSubscriber().onNext(item);
				}
				else if (this.beforeFirstEmission) {
					this.item = item;
					this.beforeFirstEmission = false;
					writeFunction.apply(this).subscribe(this.writeCompletionBarrier);
				}
				else {
					if (this.subscription != null) {
						this.subscription.cancel();
					}
					this.writeCompletionBarrier.onError(new IllegalStateException("Unexpected item."));
				}
			}
		}

		private Subscriber<? super T> requiredWriteSubscriber() {
			Assert.state(this.writeSubscriber != null, "No write subscriber");
			return this.writeSubscriber;
		}

		@Override
		public final void onError(Throwable ex) {
			if (this.readyToWrite) {
				requiredWriteSubscriber().onError(ex);
				return;
			}
			synchronized (this) {
				if (this.readyToWrite) {
					requiredWriteSubscriber().onError(ex);
				}
				else if (this.beforeFirstEmission) {
					this.beforeFirstEmission = false;
					this.writeCompletionBarrier.onError(ex);
				}
				else {
					this.error = ex;
				}
			}
		}

		@Override
		public final void onComplete() {
			if (this.readyToWrite) {
				requiredWriteSubscriber().onComplete();
				return;
			}
			synchronized (this) {
				if (this.readyToWrite) {
					requiredWriteSubscriber().onComplete();
				}
				else if (this.beforeFirstEmission) {
					this.completed = true;
					this.beforeFirstEmission = false;
					writeFunction.apply(this).subscribe(this.writeCompletionBarrier);
				}
				else {
					this.completed = true;
				}
			}
		}

		@Override
		public Context currentContext() {
			return this.writeCompletionBarrier.currentContext();
		}


		// Subscription methods (we're the Subscription to the writeSubscriber)..

		@Override
		public void request(long n) {
			Subscription s = this.subscription;
			if (s == null) {
				return;
			}
			if (this.readyToWrite) {
				s.request(n);
				return;
			}
			synchronized (this) {
				if (this.writeSubscriber != null) {
					this.readyToWrite = true;
					if (emitCachedSignals()) {
						return;
					}
					n--;
					if (n == 0) {
						return;
					}
				}
			}
			s.request(n);
		}

		private boolean emitCachedSignals() {
			if (this.item != null) {
				requiredWriteSubscriber().onNext(this.item);
			}
			if (this.error != null) {
				requiredWriteSubscriber().onError(this.error);
				return true;
			}
			if (this.completed) {
				requiredWriteSubscriber().onComplete();
				return true;
			}
			return false;
		}

		@Override
		public void cancel() {
			Subscription s = this.subscription;
			if (s != null) {
				this.subscription = null;
				s.cancel();
			}
		}


		// Publisher<T> methods (we're the Publisher to the writeSubscriber)..

		@Override
		public void subscribe(Subscriber<? super T> writeSubscriber) {
			synchronized (this) {
				Assert.state(this.writeSubscriber == null, "Only one write subscriber supported");
				this.writeSubscriber = writeSubscriber;
				if (this.error != null || this.completed) {
					this.writeSubscriber.onSubscribe(Operators.emptySubscription());
					emitCachedSignals();
				}
				else {
					this.writeSubscriber.onSubscribe(this);
				}
			}
		}
	}


	/**
	 * We need an extra barrier between the WriteBarrier itself and the actual
	 * completion subscriber.
	 *
	 * <p>The completionSubscriber is subscribed initially to the WriteBarrier.
	 * Later after the first signal is received, we need one more subscriber
	 * instance (per spec can only subscribe once) to subscribe to the write
	 * function and switch to delegating completion signals from it.
	 */
	private class WriteCompletionBarrier implements CoreSubscriber<Void>, Subscription {

		/* Downstream write completion subscriber */
		private final CoreSubscriber<? super Void> completionSubscriber;

		private final WriteBarrier writeBarrier;


		public WriteCompletionBarrier(CoreSubscriber<? super Void> subscriber, WriteBarrier writeBarrier) {
			this.completionSubscriber = subscriber;
			this.writeBarrier = writeBarrier;
		}


		/**
		 * Connect the underlying completion subscriber to this barrier in order
		 * to track cancel signals and pass them on to the write barrier.
		 */
		public void connect() {
			this.completionSubscriber.onSubscribe(this);
		}

		// Subscriber methods (we're the subscriber to the write function)..

		@Override
		public void onSubscribe(Subscription subscription) {
			subscription.request(Long.MAX_VALUE);
		}

		@Override
		public void onNext(Void aVoid) {
		}

		@Override
		public void onError(Throwable ex) {
			this.completionSubscriber.onError(ex);
		}

		@Override
		public void onComplete() {
			this.completionSubscriber.onComplete();
		}

		@Override
		public Context currentContext() {
			return this.completionSubscriber.currentContext();
		}


		@Override
		public void request(long n) {
			// Ignore: we don't produce data
		}

		@Override
		public void cancel() {
			this.writeBarrier.cancel();
		}
	}

}
