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
      ctx => null
    ),
    Field(
      "updateDeveloper",
      OptionType(DeveloperIdType),
      Some("Update a developer using modifier and id"),
      DeveloperModifierArg :: DeveloperKeyArg :: Nil,
      ctx => null
    ),
    Field(
      "deleteDevelopers",
      ListType(DeveloperIdType),
      Some("Delete developers using filter"),
      DeveloperFilterArg :: Nil,
      ctx => null
    ),
    Field(
      "deleteDeveloper",
      OptionType(DeveloperIdType),
      Some("Delete a developer using id"),
      DeveloperKeyArg :: Nil,
      ctx => null
    )
  )

  def ProjectMutation[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] = fields[SangriaContext[F], Unit](
    Field(
      "createProject",
      ProjectIdType,
      Some("Create a project using modifier"),
      ProjectModifierArg :: Nil,
      ctx => null
    ),
    Field(
      "updateProjects",
      ListType(ProjectIdType),
      Some("Update projects using modifier and filter"),
      ProjectModifierArg :: ProjectFilterArg :: Nil,
      ctx => null
    ),
    Field(
      "updateProject",
      OptionType(ProjectIdType),
      Some("Update a project using modifier and id"),
      ProjectModifierArg :: ProjectKeyArg :: Nil,
      ctx => null
    ),
    Field(
      "deleteProjects",
      ListType(ProjectIdType),
      Some("Delete projects using filter"),
      ProjectFilterArg :: Nil,
      ctx => null
    ),
    Field(
      "deleteProject",
      OptionType(ProjectIdType),
      Some("Delete a project using id"),
      ProjectKeyArg :: Nil,
      ctx => null
    )
  )

  def TaskMutation[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] = fields[SangriaContext[F], Unit](
    Field(
      "createTask",
      TaskIdType,
      Some("Create a task using modifier"),
      TaskModifierArg :: Nil,
      ctx => null
    ),
    Field(
      "updateTasks",
      ListType(TaskIdType),
      Some("Update tasks using modifier and filter"),
      TaskModifierArg :: TaskFilterArg :: Nil,
      ctx => null
    ),
    Field(
      "updateTask",
      OptionType(TaskIdType),
      Some("Update a task using modifier and id"),
      TaskModifierArg :: TaskKeyArg :: Nil,
      ctx => null
    ),
    Field(
      "deleteTasks",
      ListType(TaskIdType),
      Some("Delete tasks using filter"),
      TaskFilterArg :: Nil,
      ctx => null
    ),
    Field(
      "TaskIdType",
      OptionType(TaskIdType),
      Some("Delete a task using id"),
      TaskKeyArg :: Nil,
      ctx => null
    )
  )
}
