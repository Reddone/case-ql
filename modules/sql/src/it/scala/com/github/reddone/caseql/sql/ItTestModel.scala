package com.github.reddone.caseql.sql

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.{FieldSet, Table, TableFilter, TableLink, TableModifier}
import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.TableLink.Aux
import doobie.implicits._
import javasql._
import javatime._
import shapeless.{cachedImplicit, LabelledGeneric, TypeOf}

object ItTestModel {
  import Tables._
  import Links._
  import Filters._
  import Modifiers._

  // DEVELOPER

  final case class Developer(
      id: Long,
      name: String,
      age: Int,
      teamLeaderId: Option[Long]
  )

  object Developer {
    implicit val lgen: TypeOf.`LabelledGeneric[Developer]`.type =
      cachedImplicit
  }

  final case class DeveloperKey(id: Long)

  object DeveloperKey {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperKey]`.type =
      cachedImplicit
  }

  final case class DeveloperFilter(
      id: Option[LongFilter],
      name: Option[StringFilter],
      age: Option[IntFilter],
      teamLeaderId: Option[LongFilterOption],
      selfRelation: Option[RelationFilter[Developer, Developer, DeveloperFilter]],
      projectRelation: Option[RelationFilter[Developer, Project, ProjectFilter]],
      AND: Option[Seq[DeveloperFilter]],
      OR: Option[Seq[DeveloperFilter]],
      NOT: Option[DeveloperFilter]
  ) extends EntityFilter[DeveloperFilter]

  object DeveloperFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperFilter]`.type =
      cachedImplicit
  }

  final case class DeveloperModifier(
      id: Option[LongModifier],
      name: Option[StringModifier],
      age: Option[IntModifier],
      teamLeaderId: Option[LongModifierOption]
  ) extends EntityModifier[DeveloperModifier]

  object DeveloperModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperModifier]`.type =
      cachedImplicit
  }

  // PROJECT

  final case class Project(
      id: Long,
      title: String,
      description: Option[String]
  )

  object Project {
    implicit val lgen: TypeOf.`LabelledGeneric[Project]`.type =
      cachedImplicit
  }

  final case class ProjectKey(id: Long)

  object ProjectKey {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectKey]`.type =
      cachedImplicit
  }

  final case class ProjectFilter(
      id: Option[LongFilter],
      title: Option[StringFilter],
      description: Option[StringFilterOption],
      taskRelation: Option[RelationFilter[Project, Task, TaskFilter]],
      AND: Option[Seq[ProjectFilter]],
      OR: Option[Seq[ProjectFilter]],
      NOT: Option[ProjectFilter]
  ) extends EntityFilter[ProjectFilter]

  object ProjectFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectFilter]`.type =
      cachedImplicit
  }

  final case class ProjectModifier(
      id: Option[LongModifier],
      title: Option[StringModifier],
      description: Option[StringModifierOption]
  ) extends EntityModifier[ProjectModifier]

  object ProjectModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectModifier]`.type =
      cachedImplicit
  }

  // DEVELOPER_PROJECT_LINK

  final case class DeveloperProjectLink(
      developerId: Long,
      projectId: Long
  )

  object DeveloperProjectLink {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLink]`.type =
      cachedImplicit
  }

  final case class DeveloperProjectLinkKey(developerId: Long, projectId: Long)

  object DeveloperProjectLinkKey {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLinkKey]`.type =
      cachedImplicit
  }

  final case class DeveloperProjectLinkFilter(
      developerId: Option[LongFilter],
      projectId: Option[LongFilter],
      AND: Option[Seq[DeveloperProjectLinkFilter]],
      OR: Option[Seq[DeveloperProjectLinkFilter]],
      NOT: Option[DeveloperProjectLinkFilter]
  ) extends EntityFilter[DeveloperProjectLinkFilter]

  object DeveloperProjectLinkFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLinkFilter]`.type =
      cachedImplicit
  }

  final case class DeveloperProjectLinkModifier(
      developerId: Option[LongModifier],
      projectId: Option[LongModifier]
  ) extends EntityModifier[DeveloperProjectLinkModifier]

  object DeveloperProjectLinkModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLinkModifier]`.type =
      cachedImplicit
  }

  // TASK

  final case class Task(
      id: Long,
      label: String,
      description: Option[String],
      deadline: Timestamp,
      projectId: Long
  )

  object Task {
    implicit val lgen: TypeOf.`LabelledGeneric[Task]`.type =
      cachedImplicit
  }

  final case class TaskKey(id: Long)

  object TaskKey {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskKey]`.type =
      cachedImplicit
  }

  final case class TaskFilter(
      id: Option[LongFilter],
      label: Option[StringFilter],
      description: Option[StringFilterOption],
      deadline: Option[TimestampFilter],
      projectId: Option[LongFilter],
      AND: Option[Seq[TaskFilter]],
      OR: Option[Seq[TaskFilter]],
      NOT: Option[TaskFilter]
  ) extends EntityFilter[TaskFilter]

  object TaskFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskFilter]`.type =
      cachedImplicit
  }

  final case class TaskModifier(
      id: Option[LongModifier],
      label: Option[StringModifier],
      description: Option[StringModifierOption],
      deadline: Option[TimestampModifier],
      projectId: Option[LongModifier]
  ) extends EntityModifier[TaskModifier]

  object TaskModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskModifier]`.type =
      cachedImplicit
  }

  // TABLES

  object Tables {

    implicit val developerTable: Table[Developer, DeveloperKey] =
      Table.derive[Developer, DeveloperKey]()

    implicit val projectTable: Table[Project, ProjectKey] =
      Table.derive[Project, ProjectKey]()

    implicit val dplTable: Table[DeveloperProjectLink, DeveloperProjectLinkKey] =
      Table.derive[DeveloperProjectLink, DeveloperProjectLinkKey]()

    implicit val taskTable: Table[Task, TaskKey] =
      Table.derive[Task, TaskKey]()
  }

  // LINKS

  object Links {

    implicit val developerSelfLink: Aux[Developer, Developer, Unit] =
      TableLink.self[Developer](
        FieldSet("id"),
        FieldSet("teamLeaderId")
      )

    implicit val developerDplLink: Aux[Developer, DeveloperProjectLink, Unit] =
      TableLink.direct[Developer, DeveloperProjectLink](
        FieldSet("id"),
        FieldSet("developerId")
      )

    implicit val projectDplLink: Aux[Project, DeveloperProjectLink, Unit] =
      TableLink.direct[Project, DeveloperProjectLink](
        FieldSet("id"),
        FieldSet("projectId")
      )

    implicit val taskLink: Aux[Project, Task, Unit] =
      TableLink.direct[Project, Task](
        FieldSet("id"),
        FieldSet("projectId")
      )

    implicit val developerProjectLink: Aux[Developer, Project, DeveloperProjectLink] =
      TableLink.union(developerDplLink, projectDplLink)
  }

  // FILTERS

  object Filters {

    implicit lazy val developerFilter: TableFilter[Developer, DeveloperFilter] =
      TableFilter.derive[Developer, DeveloperFilter]()

    implicit val projectFilter: TableFilter[Project, ProjectFilter] =
      TableFilter.derive[Project, ProjectFilter]()

    implicit val dplFilter: TableFilter[DeveloperProjectLink, DeveloperProjectLinkFilter] =
      TableFilter.derive[DeveloperProjectLink, DeveloperProjectLinkFilter]()

    implicit val taskFilter: TableFilter[Task, TaskFilter] =
      TableFilter.derive[Task, TaskFilter]()
  }

  // MODIFIERS

  object Modifiers {

    implicit val developerModifier: TableModifier[Developer, DeveloperModifier] =
      TableModifier.derive[Developer, DeveloperModifier]()

    implicit val projectModifier: TableModifier[Project, ProjectModifier] =
      TableModifier.derive[Project, ProjectModifier]()

    implicit val dplModifier: TableModifier[DeveloperProjectLink, DeveloperProjectLinkModifier] =
      TableModifier.derive[DeveloperProjectLink, DeveloperProjectLinkModifier]()

    implicit val taskModifier: TableModifier[Task, TaskModifier] =
      TableModifier.derive[Task, TaskModifier]()
  }
}
