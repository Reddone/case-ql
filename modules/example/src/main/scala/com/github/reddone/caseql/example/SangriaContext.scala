package com.github.reddone.caseql.example

import cats._
import cats.effect._
import com.github.reddone.caseql.example.service._
import doobie.util.transactor.Transactor

final case class SangriaContext[F[_]: Effect](
    developerService: DeveloperService[F],
    projectService: ProjectService[F],
    taskService: TaskService[F]
)

object SangriaContext {

  def production[F[_]: Effect: Monad](xa: Transactor[F]): SangriaContext[F] = {
    SangriaContext(
      DeveloperService.production(xa),
      ProjectService.production(xa),
      TaskService.production(xa)
    )
  }
}
