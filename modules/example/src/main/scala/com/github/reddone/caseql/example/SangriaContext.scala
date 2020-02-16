package com.github.reddone.caseql.example

import cats.effect.Effect
import doobie.util.transactor.Transactor

import scala.concurrent.ExecutionContext

final case class SangriaContext()(implicit val ec: ExecutionContext)

object SangriaContext {

  def production[F[_]: Effect](xa: Transactor[F])(implicit ec: ExecutionContext): SangriaContext = {
    SangriaContext()
  }
}
