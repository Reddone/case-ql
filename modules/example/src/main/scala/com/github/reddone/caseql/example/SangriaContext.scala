package com.github.reddone.caseql.example

import cats.effect.Effect
import com.github.reddone.caseql.example.service._
import doobie.util.transactor.Transactor

final case class SangriaContext[F[_]: Effect](
    developerService: DeveloperService[F],
    projectService: ProjectService[F],
    taskService: TaskService[F]
)

object SangriaContext {

  def production[F[_]: Effect](xa: Transactor[F]): SangriaContext[F] = {
    SangriaContext(
      DeveloperService.production(xa),
      ProjectService.production(xa),
      TaskService.production(xa)
    )
  }
}
