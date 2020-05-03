package com.github.reddone.caseql.example

import cats.effect.Effect
import cats.implicits._
import sangria.execution.deferred.{Deferred, DeferredResolver, Fetcher}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.Success

object SangriaResolver {

  def apply[F[_]: Effect]: DeferredResolver[SangriaContext[F]] =
    DeferredResolver.fetchersWithFallback(
      fallback = Resolvers[F],
      fetchers = Fetchers[F]: _*
    )

  def Fetchers[F[_]: Effect]: List[Fetcher[SangriaContext[F], _, _, _]] =
    List.empty[Fetcher[SangriaContext[F], _, _, _]]

  def Resolvers[F[_]: Effect]: DeferredResolver[SangriaContext[F]] =
    new DeferredResolver[SangriaContext[F]] {
      override def resolve(
          deferred: Vector[Deferred[Any]],
          ctx: SangriaContext[F],
          queryState: Any
      )(implicit ec: ExecutionContext): Vector[Future[Any]] = {
        val promises: Map[Deferred[Any], Promise[Any]] =
          deferred.map(d => d -> Promise[Any]()).toMap

        def select[A <: Deferred[Any]: ClassTag]: List[A] =
          promises.keys.collect { case a: A => a }.toList

        def complete[A](d: Deferred[A], a: A): F[Unit] =
          Effect[F].delay(promises(d).complete(Success(a))).void

        def fail[A](d: Deferred[A], t: Throwable): F[Unit] =
          Effect[F].delay(promises(d).failure(t)).void

        deferred.map(promises(_).future)
      }
    }
}
