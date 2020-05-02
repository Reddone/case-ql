package com.github.reddone.caseql.example.service

import cats.effect.Effect
import doobie.util.transactor.Transactor

trait ProjectService[F[_]] {}

object ProjectService {

  def fromTransactor[F[_]: Effect](xa: Transactor[F]): ProjectService[F] = new ProjectService[F] {}
}
