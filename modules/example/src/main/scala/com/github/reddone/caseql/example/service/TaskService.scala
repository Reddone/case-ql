package com.github.reddone.caseql.example.service

import cats.effect.Effect
import doobie.util.transactor.Transactor

trait TaskService[F[_]] {}

object TaskService {

  def fromTransactor[F[_]: Effect](xa: Transactor[F]): TaskService[F] = new TaskService[F] {}
}
