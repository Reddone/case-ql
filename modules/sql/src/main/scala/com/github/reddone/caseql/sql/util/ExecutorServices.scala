package com.github.reddone.caseql.sql.util

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}

import cats.effect.{Resource, Sync}

object ExecutorServices {

  // CACHED

  def cachedThreadPool[F[_]](
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newCachedThreadPool)
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  def cachedThreadPool[F[_]](factory: ThreadFactory)(
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newCachedThreadPool(factory))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  // FIXED

  def fixedThreadPool[F[_]](size: Int)(
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newFixedThreadPool(size))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  def fixedThreadPool[F[_]](size: Int, factory: ThreadFactory)(
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newFixedThreadPool(size, factory))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  // WORK STEALING

  def workStealingThreadPool[F[_]](
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newWorkStealingPool)
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  def workStealingThreadPool[F[_]](parallelism: Int)(
      implicit sf: Sync[F]
  ): Resource[F, ExecutorService] = {
    val alloc = sf.delay(Executors.newWorkStealingPool(parallelism))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  // SCHEDULED

  def scheduledThreadPool[F[_]](size: Int)(
      implicit sf: Sync[F]
  ): Resource[F, ScheduledExecutorService] = {
    val alloc = sf.delay(Executors.newScheduledThreadPool(size))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }

  def scheduledThreadPool[F[_]](size: Int, factory: ThreadFactory)(
      implicit sf: Sync[F]
  ): Resource[F, ScheduledExecutorService] = {
    val alloc = sf.delay(Executors.newScheduledThreadPool(size, factory))
    val free  = (es: ExecutorService) => sf.delay(es.shutdown())
    Resource.make(alloc)(free)
  }
}
