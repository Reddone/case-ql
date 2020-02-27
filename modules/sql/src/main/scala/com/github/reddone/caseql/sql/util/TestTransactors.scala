package com.github.reddone.caseql.sql.util

import java.util.concurrent.Executors

import cats.effect.{Async, Blocker, ContextShift}
import com.github.reddone.caseql.sql.config.DoobieConfig
import doobie.HC
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import doobie.util.transactor.Transactor.Aux

import scala.concurrent.ExecutionContext

object TestTransactors {

  object BlockerMode extends Enumeration {
    val Cached: BlockerMode.Value = Value("cached")
    val Fixed: BlockerMode.Value  = Value("fixed")
    val Sync: BlockerMode.Value   = Value("sync")
  }

  def alwaysRollback[F[_]: Async: ContextShift](xa: Transactor[F]): Transactor[F] = {
    Transactor.after.set(xa, HC.rollback)
  }

  def valueOf[F[_]: Async: ContextShift](config: DoobieConfig, mode: BlockerMode.Value): Aux[F, Unit] = {
    val DoobieConfig(numThreads, driverClassName, url, user, password) = config
    mode match {
      case BlockerMode.Cached => cachedDriverTransactor(driverClassName, url, user, password)
      case BlockerMode.Fixed  => fixedDriverTransactor(numThreads, driverClassName, url, user, password)
      case BlockerMode.Sync   => syncDriverTransactor(driverClassName, url, user, password)
    }
  }

  private def cachedDriverTransactor[F[_]: Async: ContextShift](
      driverClassName: String,
      url: String,
      user: String,
      password: String
  ): Aux[F, Unit] = {
    Transactor.fromDriverManager[F](
      driverClassName,
      url,
      user,
      password,
      Blocker.liftExecutionContext(
        ExecutionContext.fromExecutor(
          Executors.newCachedThreadPool((r: Runnable) => {
            val th = new Thread(r)
            th.setName(s"doobie-fromDriverManager-pool-${th.getId}")
            th.setDaemon(true)
            th
          })
        )
      )
    )
  }

  private def fixedDriverTransactor[F[_]: Async: ContextShift](
      numThreads: Int,
      driverClassName: String,
      url: String,
      user: String,
      password: String
  ): Aux[F, Unit] = {
    Transactor.fromDriverManager[F](
      driverClassName,
      url,
      user,
      password,
      Blocker.liftExecutionContext(
        ExecutionContext.fromExecutor(
          Executors.newFixedThreadPool(
            numThreads,
            (r: Runnable) => {
              val th = new Thread(r)
              th.setName(s"doobie-fromDriverManager-pool-${th.getId}")
              th.setDaemon(true)
              th
            }
          )
        )
      )
    )
  }

  private def syncDriverTransactor[F[_]: Async: ContextShift](
      driverClassName: String,
      url: String,
      user: String,
      password: String
  ): Aux[F, Unit] = {
    Transactor.fromDriverManager[F](
      driverClassName,
      url,
      user,
      password,
      Blocker.liftExecutionContext(ExecutionContexts.synchronous)
    )
  }
}
