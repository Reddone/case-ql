package com.github.reddone.caseql.sql

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.{FieldSet, Table, TableFilter, TableLink, TableModifier}
import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.TableLink.Aux
import com.github.reddone.caseql.sql.util.CirceDecoders._
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import shapeless.{cachedImplicit, LabelledGeneric, TypeOf}

object ItTestModel {

  // DEVELOPER ENTITY AND LINK

  final case class Developer(
      id: Long,
      fullName: String,
      age: Int,
      teamLeaderId: Option[Long]
  )

  object Developer {
    implicit val lgen: TypeOf.`LabelledGeneric[Developer]`.type =
      cachedImplicit
    implicit val decoder: Decoder[Developer] =
      deriveDecoder[Developer]
    implicit val table: Table[Developer, DeveloperKey] =
      Table.derive[Developer, DeveloperKey]()
  }

  final case class DeveloperKey(id: Long)

  object DeveloperKey {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperKey]`.type =
      cachedImplicit
    implicit val decoder: Decoder[DeveloperKey] =
      deriveDecoder[DeveloperKey]
  }

  // DEVELOPER FILTER

  final case class DeveloperFilter(
      id: Option[LongFilter],
      fullName: Option[StringFilter],
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
    implicit val decoder: Decoder[DeveloperFilter] =
      deriveDecoder[DeveloperFilter]
    implicit lazy val tableFilter: TableFilter[Developer, DeveloperFilter] =
      TableFilter.derive[Developer, DeveloperFilter]()
  }

  // DEVELOPER MODIFIER

  final case class DeveloperModifier(
      id: Option[LongModifier],
      fullName: Option[StringModifier],
      age: Option[IntModifier],
      teamLeaderId: Option[LongModifierOption]
  ) extends EntityModifier[DeveloperModifier]

  object DeveloperModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperModifier]`.type =
      cachedImplicit
    implicit val decoder: Decoder[DeveloperModifier] =
      deriveDecoder[DeveloperModifier]
    implicit val tableModifier: TableModifier[Developer, DeveloperModifier] =
      TableModifier.derive[Developer, DeveloperModifier]()
  }

  // PROJECT ENTITY AND LINK

  final case class Project(
      id: Long,
      title: String,
      description: Option[String],
      createdAt: Timestamp,
      updatedAt: Timestamp
  )

  object Project {
    implicit val lgen: TypeOf.`LabelledGeneric[Project]`.type =
      cachedImplicit
    implicit val decoder: Decoder[Project] =
      deriveDecoder[Project]
    implicit val table: Table[Project, ProjectKey] =
      Table.derive[Project, ProjectKey]()
  }

  final case class ProjectKey(id: Long)

  object ProjectKey {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectKey]`.type =
      cachedImplicit
    implicit val decoder: Decoder[ProjectKey] =
      deriveDecoder[ProjectKey]
  }

  // PROJECT FILTER

  final case class ProjectFilter(
      id: Option[LongFilter],
      title: Option[StringFilter],
      description: Option[StringFilterOption],
      createdAt: Option[TimestampFilter],
      updatedAt: Option[TimestampFilter],
      taskRelation: Option[RelationFilter[Project, Task, TaskFilter]],
      AND: Option[Seq[ProjectFilter]],
      OR: Option[Seq[ProjectFilter]],
      NOT: Option[ProjectFilter]
  ) extends EntityFilter[ProjectFilter]

  object ProjectFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectFilter]`.type =
      cachedImplicit
    implicit val decoder: Decoder[ProjectFilter] =
      deriveDecoder[ProjectFilter]
    implicit val tableFilter: TableFilter[Project, ProjectFilter] =
      TableFilter.derive[Project, ProjectFilter]()
  }

  // PROJECT MODIFIER

  final case class ProjectModifier(
      id: Option[LongModifier],
      title: Option[StringModifier],
      description: Option[StringModifierOption],
      createdAt: Option[TimestampModifier],
      updatedAt: Option[TimestampModifier]
  ) extends EntityModifier[ProjectModifier]

  object ProjectModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[ProjectModifier]`.type =
      cachedImplicit
    implicit val decoder: Decoder[ProjectModifier] =
      deriveDecoder[ProjectModifier]
    implicit val tableModifier: TableModifier[Project, ProjectModifier] =
      TableModifier.derive[Project, ProjectModifier]()
  }

  // DEVELOPER_PROJECT_LINK ENTITY AND LINK

  final case class DeveloperProjectLink(
      developerId: Long,
      projectId: Long
  )

  object DeveloperProjectLink {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLink]`.type =
      cachedImplicit
    implicit val decoder: Decoder[DeveloperProjectLink] =
      deriveDecoder[DeveloperProjectLink]
    implicit val table: Table[DeveloperProjectLink, DeveloperProjectLinkKey] =
      Table.derive[DeveloperProjectLink, DeveloperProjectLinkKey]()
  }

  final case class DeveloperProjectLinkKey(developerId: Long, projectId: Long)

  object DeveloperProjectLinkKey {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLinkKey]`.type =
      cachedImplicit
    implicit val decoder: Decoder[DeveloperProjectLinkKey] =
      deriveDecoder[DeveloperProjectLinkKey]
  }

  // DEVELOPER_PROJECT_LINK FILTER

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
    implicit val decoder: Decoder[DeveloperProjectLinkFilter] =
      deriveDecoder[DeveloperProjectLinkFilter]
    implicit val tableFilter: TableFilter[DeveloperProjectLink, DeveloperProjectLinkFilter] =
      TableFilter.derive[DeveloperProjectLink, DeveloperProjectLinkFilter]()
  }

  // DEVELOPER_PROJECT_LINK MODIFIER

  final case class DeveloperProjectLinkModifier(
      developerId: Option[LongModifier],
      projectId: Option[LongModifier]
  ) extends EntityModifier[DeveloperProjectLinkModifier]

  object DeveloperProjectLinkModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[DeveloperProjectLinkModifier]`.type =
      cachedImplicit
    implicit val decoder: Decoder[DeveloperProjectLinkModifier] =
      deriveDecoder[DeveloperProjectLinkModifier]
    implicit val tableModifier: TableModifier[DeveloperProjectLink, DeveloperProjectLinkModifier] =
      TableModifier.derive[DeveloperProjectLink, DeveloperProjectLinkModifier]()
  }

  // TASK ENTITY

  final case class Task(
      id: Long,
      label: String,
      description: String,
      duration: Timestamp,
      projectId: Long
  )

  object Task {
    implicit val lgen: TypeOf.`LabelledGeneric[Task]`.type =
      cachedImplicit
    implicit val decoder: Decoder[Task] =
      deriveDecoder[Task]
    implicit val table: Table[Task, TaskKey] =
      Table.derive[Task, TaskKey]()
  }

  final case class TaskKey(id: Long)

  object TaskKey {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskKey]`.type =
      cachedImplicit
    implicit val decoder: Decoder[TaskKey] =
      deriveDecoder[TaskKey]
  }

  // TASK FILTER

  final case class TaskFilter(
      id: Option[LongFilter],
      label: Option[StringFilter],
      description: Option[StringFilter],
      duration: Option[TimestampFilter],
      projectId: Option[LongFilter],
      AND: Option[Seq[TaskFilter]],
      OR: Option[Seq[TaskFilter]],
      NOT: Option[TaskFilter]
  ) extends EntityFilter[TaskFilter]

  object TaskFilter {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskFilter]`.type =
      cachedImplicit
    implicit val decoder: Decoder[TaskFilter] =
      deriveDecoder[TaskFilter]
    implicit val tableFilter: TableFilter[Task, TaskFilter] =
      TableFilter.derive[Task, TaskFilter]()
  }

  // TASK MODIFIER

  final case class TaskModifier(
      id: Option[LongModifier],
      label: Option[StringModifier],
      description: Option[StringModifier],
      duration: Option[TimestampModifier],
      projectId: Option[LongModifier]
  ) extends EntityModifier[TaskModifier]

  object TaskModifier {
    implicit val lgen: TypeOf.`LabelledGeneric[TaskModifier]`.type =
      cachedImplicit
    implicit val decoder: Decoder[TaskModifier] =
      deriveDecoder[TaskModifier]
    implicit val tableModifier: TableModifier[Task, TaskModifier] =
      TableModifier.derive[Task, TaskModifier]()
  }

  // LINKS

  implicit val selfLink: Aux[Developer, Developer, Unit] =
    TableLink.self[Developer](
      FieldSet("id"),
      FieldSet("teamLeaderId")
    )
  implicit val projectLink: Aux[Developer, DeveloperProjectLink, Unit] =
    TableLink.direct[Developer, DeveloperProjectLink](
      FieldSet("id"),
      FieldSet("developerId")
    )

  implicit val developerLink: Aux[Project, DeveloperProjectLink, Unit] =
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
    TableLink.union(projectLink, developerLink)
}
