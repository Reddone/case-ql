package com.github.reddone.caseql.example

import cats.effect.Effect
import cats.implicits._
import sangria.execution.deferred.{Deferred, DeferredResolver, Fetcher}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.Success

object SangriaResolver {

  def apply[F[_]: Effect]: DeferredResolver[SangriaContext[F]] = {
    val fetchers = Seq.empty[Fetcher[SangriaContext[F], _, _, _]]
    DeferredResolver.fetchers(fetchers: _*)
  }

  def deferredResolver[F[_]: Effect]: DeferredResolver[SangriaContext[F]] =
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

        def failAll[A](ds: List[Deferred[A]]): PartialFunction[Throwable, Future[Unit]] = {
          case t: Throwable =>
            ds.map(fail(_, t))
            Future.unit
        }

        /*
        // Complete a bunch of countries by doing a batch database query
        def completeCountries(codes: List[CountryType.Deferred.ByCode]): F[Unit] =
          for {
            cs <- ctx.country.fetchByCodes(codes.map(_.code))
            _  <- cs.traverse(c => complete(CountryType.Deferred.ByCode(c.code), c))
          } yield ()

        // Complete a bunch of languages by doing a batch database query
        def completeLanguages(codes: List[LanguageType.Deferred.ByCountryCode]): F[Unit] =
          for {
            m <- ctx.language.fetchByCountryCodes(codes.map(_.code))
            _ <- m.toList.traverse { case (c, ls) => complete(LanguageType.Deferred.ByCountryCode(c), ls) }
          } yield ()
         */

        //completeCountries(select[CountryType.Deferred.ByCode]).toIO.unsafeToFuture
        //completeLanguages(select[LanguageType.Deferred.ByCountryCode]).toIO.unsafeToFuture

        deferred.map(promises(_).future)
      }
    }
}
