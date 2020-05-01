package com.github.reddone.caseql.example

import cats.effect.Async
import doobie.util.transactor.Transactor

import scala.concurrent.ExecutionContext

final case class SangriaContext()(implicit val ec: ExecutionContext)

object SangriaContext {

  def production[F[_]: Async](xa: Transactor[F])(implicit ec: ExecutionContext): SangriaContext = {
    SangriaContext()
  }
}
