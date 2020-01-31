package com.github.reddone.caseql.example.repository

import cats.effect.{Async, Blocker, ContextShift, Resource}
import com.github.reddone.caseql.sql.config.DoobieConfig
import com.typesafe.config.Config
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

object TransactorResource {

  def create[F[_]: Async: ContextShift](config: Config): Resource[F, HikariTransactor[F]] = {
    val doobieConfig = DoobieConfig.valueOf(config)

    for {
      tec <- ExecutionContexts.cachedThreadPool[F]
      cec <- ExecutionContexts.fixedThreadPool[F](doobieConfig.numThreads)
      xa <- HikariTransactor.newHikariTransactor[F](
        doobieConfig.driverClassName,
        doobieConfig.url,
        doobieConfig.user,
        doobieConfig.password,
        tec,
        Blocker.liftExecutionContext(cec)
      )
    } yield xa
  }
}
