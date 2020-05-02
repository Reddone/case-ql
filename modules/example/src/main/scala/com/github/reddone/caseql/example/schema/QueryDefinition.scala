package com.github.reddone.caseql.example.schema

import cats.effect.Effect
import cats.effect.implicits._
import com.github.reddone.caseql.example.SangriaContext
import com.github.reddone.caseql.example.schema.ArgumentDefinition._
import com.github.reddone.caseql.example.schema.ObjectDefinition._
import sangria.schema._
import sangria.schema.ReduceAction._

object QueryDefinition {

  def apply[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    DeveloperQuery ++ ProjectQuery ++ TaskQuery

  def DeveloperQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "findDevelopers",
        ListType(DeveloperType[F]),
        Some("Find all developers using filter"),
        DeveloperFilterArg :: Nil,
        ctx =>
          ctx.ctx.developerService
            .getDevelopers(ctx.arg(DeveloperFilterArg))
            .toIO
            .unsafeToFuture()
      ),
      Field(
        "findDeveloper",
        OptionType(DeveloperType),
        Some("Find a developer using id"),
        DeveloperKeyArg :: Nil,
        ctx =>
          ctx.ctx.developerService
            .getDeveloperById(ctx.arg(DeveloperKeyArg))
            .toIO
            .unsafeToFuture()
      )
    )

  def ProjectQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "findProjects",
        ListType(ProjectType),
        Some("Find all projects using filter"),
        ProjectFilterArg :: Nil,
        ctx =>
          ctx.ctx.projectService
            .getProjects(ctx.arg(ProjectFilterArg))
            .toIO
            .unsafeToFuture()
      ),
      Field(
        "findProject",
        OptionType(ProjectType),
        Some("Find a project using id"),
        ProjectKeyArg :: Nil,
        ctx =>
          ctx.ctx.projectService
            .getProjectById(ctx.arg(ProjectKeyArg))
            .toIO
            .unsafeToFuture()
      )
    )

  def TaskQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "findTasks",
        ListType(TaskType),
        Some("Find all tasks using filter"),
        TaskFilterArg :: Nil,
        ctx =>
          ctx.ctx.taskService
            .getTasks(ctx.arg(TaskFilterArg))
            .toIO
            .unsafeToFuture()
      ),
      Field(
        "findTask",
        OptionType(TaskType),
        Some("Find a task using id"),
        TaskKeyArg :: Nil,
        ctx =>
          ctx.ctx.taskService
            .getTaskById(ctx.arg(TaskKeyArg))
            .toIO
            .unsafeToFuture()
      )
    )
}
