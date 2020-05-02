package com.github.reddone.caseql.example

import cats.effect.Effect
import sangria.execution.deferred.{DeferredResolver, Fetcher}

object SangriaResolver {

  def apply[F[_]: Effect]: DeferredResolver[SangriaContext[F]] = {
    val fetchers = Seq.empty[Fetcher[SangriaContext[F], _, _, _]]
    DeferredResolver.fetchers(fetchers: _*)
  }
}
