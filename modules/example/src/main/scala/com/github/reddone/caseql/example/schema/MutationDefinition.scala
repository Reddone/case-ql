package com.github.reddone.caseql.example.schema

import cats.effect.Effect
import cats.effect.implicits._
import com.github.reddone.caseql.example.SangriaContext
import com.github.reddone.caseql.example.schema.ArgumentDefinition._
import com.github.reddone.caseql.example.schema.ObjectDefinition._
import sangria.schema._
import sangria.schema.ReduceAction._

object MutationDefinition {

  def apply[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    DeveloperMutation[F] ++ ProjectMutation[F] ++ TaskMutation[F]

  def DeveloperMutation[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] = fields[SangriaContext[F], Unit](
    Field(
      "createDeveloper",
      DeveloperIdType,
      Some("Create a developer using modifier"),
      DeveloperModifierArg :: Nil,
      ctx =>
        ctx.ctx.developerService
          .insertDeveloper(ctx.arg(DeveloperModifierArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateDevelopers",
      ListType(DeveloperIdType),
      Some("Update developers using modifier and filter"),
      DeveloperModifierArg :: DeveloperFilterArg :: Nil,
      ctx =>
        ctx.ctx.developerService
          .updateDevelopers(ctx.arg(DeveloperModifierArg), ctx.arg(DeveloperFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateDeveloper",
      OptionType(DeveloperIdType),
      Some("Update a developer using modifier and id"),
      DeveloperModifierArg :: DeveloperKeyArg :: Nil,
      ctx =>
        ctx.ctx.developerService
          .updateDeveloperById(ctx.arg(DeveloperModifierArg), ctx.arg(DeveloperKeyArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "deleteDevelopers",
      ListType(DeveloperIdType),
      Some("Delete developers using filter"),
      DeveloperFilterArg :: Nil,
      ctx =>
        ctx.ctx.developerService
          .deleteDevelopers(ctx.arg(DeveloperFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "deleteDeveloper",
      OptionType(DeveloperIdType),
      Some("Delete a developer using id"),
      DeveloperKeyArg :: Nil,
      ctx =>
        ctx.ctx.developerService
          .deleteDeveloperById(ctx.arg(DeveloperKeyArg))
          .toIO
          .unsafeToFuture()
    )
  )

  def ProjectMutation[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] = fields[SangriaContext[F], Unit](
    Field(
      "createProject",
      ProjectIdType,
      Some("Create a project using modifier"),
      ProjectModifierArg :: Nil,
      ctx =>
        ctx.ctx.projectService
          .insertProject(ctx.arg(ProjectModifierArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateProjects",
      ListType(ProjectIdType),
      Some("Update projects using modifier and filter"),
      ProjectModifierArg :: ProjectFilterArg :: Nil,
      ctx =>
        ctx.ctx.projectService
          .updateProjects(ctx.arg(ProjectModifierArg), ctx.arg(ProjectFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateProject",
      OptionType(ProjectIdType),
      Some("Update a project using modifier and id"),
      ProjectModifierArg :: ProjectKeyArg :: Nil,
      ctx =>
        ctx.ctx.projectService
          .updateProjectById(ctx.arg(ProjectModifierArg), ctx.arg(ProjectKeyArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "deleteProjects",
      ListType(ProjectIdType),
      Some("Delete projects using filter"),
      ProjectFilterArg :: Nil,
      ctx =>
        ctx.ctx.projectService
          .deleteProjects(ctx.arg(ProjectFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "deleteProject",
      OptionType(ProjectIdType),
      Some("Delete a project using id"),
      ProjectKeyArg :: Nil,
      ctx =>
        ctx.ctx.projectService
          .deleteProjectById(ctx.arg(ProjectKeyArg))
          .toIO
          .unsafeToFuture()
    )
  )

  def TaskMutation[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] = fields[SangriaContext[F], Unit](
    Field(
      "createTask",
      TaskIdType,
      Some("Create a task using modifier"),
      TaskModifierArg :: Nil,
      ctx =>
        ctx.ctx.taskService
          .insertTask(ctx.arg(TaskModifierArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateTasks",
      ListType(TaskIdType),
      Some("Update tasks using modifier and filter"),
      TaskModifierArg :: TaskFilterArg :: Nil,
      ctx =>
        ctx.ctx.taskService
          .updateTasks(ctx.arg(TaskModifierArg), ctx.arg(TaskFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "updateTask",
      OptionType(TaskIdType),
      Some("Update a task using modifier and id"),
      TaskModifierArg :: TaskKeyArg :: Nil,
      ctx =>
        ctx.ctx.taskService
          .updateTaskById(ctx.arg(TaskModifierArg), ctx.arg(TaskKeyArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "deleteTasks",
      ListType(TaskIdType),
      Some("Delete tasks using filter"),
      TaskFilterArg :: Nil,
      ctx =>
        ctx.ctx.taskService
          .deleteTasks(ctx.arg(TaskFilterArg))
          .toIO
          .unsafeToFuture()
    ),
    Field(
      "TaskIdType",
      OptionType(TaskIdType),
      Some("Delete a task using id"),
      TaskKeyArg :: Nil,
      ctx =>
        ctx.ctx.taskService
          .deleteTaskById(ctx.arg(TaskKeyArg))
          .toIO
          .unsafeToFuture()
    )
  )
}
