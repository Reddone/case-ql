package com.github.reddone.caseql.example.service

import cats.effect.Effect
import doobie.util.transactor.Transactor

trait DeveloperService[F[_]] {}

object DeveloperService {

  def fromTransactor[F[_]: Effect](xa: Transactor[F]): DeveloperService[F] = new DeveloperService[F] {}
}
