package com.github.reddone.caseql.example.schema

import cats.effect.Effect
import com.github.reddone.caseql.example.SangriaContext
import com.github.reddone.caseql.example.schema.ArgumentDefinition._
import com.github.reddone.caseql.example.schema.ObjectDefinition._
import sangria.schema._

object QueryDefinition {

  def ExampleQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    DeveloperQuery ++ ProjectQuery ++ TaskQuery

  def DeveloperQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "developers",
        ListType(DeveloperType),
        Some("Find all developers using filter"),
        DeveloperFilterArg :: Nil,
        ctx => null
      ),
      Field(
        "developer",
        OptionType(DeveloperType),
        Some("Find developer using id"),
        DeveloperKeyArg :: Nil,
        ctx => null
      )
    )

  def ProjectQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "projects",
        ListType(ProjectType),
        Some("Find all projects using filter"),
        ProjectFilterArg :: Nil,
        ctx => null
      ),
      Field(
        "project",
        OptionType(ProjectType),
        Some("Find project using id"),
        ProjectKeyArg :: Nil,
        ctx => null
      )
    )

  def TaskQuery[F[_]: Effect]: List[Field[SangriaContext[F], Unit]] =
    fields[SangriaContext[F], Unit](
      Field(
        "tasks",
        ListType(TaskType),
        Some("Find all tasks using filter"),
        TaskFilterArg :: Nil,
        ctx => null
      ),
      Field(
        "task",
        OptionType(TaskType),
        Some("Find task using id"),
        TaskKeyArg :: Nil,
        ctx => null
      )
    )
}
