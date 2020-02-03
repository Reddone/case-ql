package com.github.reddone.caseql.sql.entity

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.FilterWrapper
import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.generic.{Table, TableFilter, TableModifier}
import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.util.CirceDecoders._
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object models {

  // developer entity, filter, modifier

  final case class Developer(
      id: Long,
      fullName: String,
      age: Int
  )

  object Developer {
    implicit val decoder: Decoder[Developer] = deriveDecoder[Developer]
    implicit val table: Table[Developer] = Table.derive[Developer, DeveloperKey]()
  }

  final case class DeveloperKey(id: Long)

  object DeveloperKey {
    implicit val decoder: Decoder[DeveloperKey] = deriveDecoder[DeveloperKey]
  }

  final case class DeveloperFilter(
      id: Option[LongFilter],
      fullName: Option[StringFilter],
      age: Option[IntFilter],
      AND: Option[Seq[DeveloperFilter]],
      OR: Option[Seq[DeveloperFilter]],
      NOT: Option[DeveloperFilter]
  ) extends FilterWrapper[DeveloperFilter]

  object DeveloperFilter {
    implicit val decoder: Decoder[DeveloperFilter] = deriveDecoder[DeveloperFilter]
    implicit val tableFilter: TableFilter[Developer, DeveloperFilter] =
      TableFilter.derive[Developer, DeveloperFilter]()
  }

  final case class DeveloperModifier(
      id: Option[LongModifier],
      fullName: Option[StringModifier],
      age: Option[IntModifier]
  )

  object DeveloperModifier {
    implicit val decoder: Decoder[DeveloperModifier] = deriveDecoder[DeveloperModifier]
    implicit val tableModifier: TableModifier[Developer, DeveloperModifier] =
      TableModifier.derive[Developer, DeveloperModifier]()
  }

  // project entity, filter, modifier

  final case class Project(
      id: Long,
      title: String,
      description: Option[String],
      createdAt: Timestamp,
      updatedAt: Timestamp
  )

  object Project {
    implicit val decoder: Decoder[Project] = deriveDecoder[Project]
    implicit val table: Table[Project] = Table.derive[Project, ProjectKey]()
  }

  final case class ProjectKey(id: Long)

  object ProjectKey {
    implicit val decoder: Decoder[ProjectKey] = deriveDecoder[ProjectKey]
  }

  final case class ProjectFilter(
      id: Option[LongFilter],
      title: Option[StringFilter],
      description: Option[StringFilterOption],
      createdAt: Option[TimestampFilter],
      updatedAt: Option[TimestampFilter],
      AND: Option[Seq[ProjectFilter]],
      OR: Option[Seq[ProjectFilter]],
      NOT: Option[ProjectFilter]
  ) extends FilterWrapper[ProjectFilter]

  object ProjectFilter {
    implicit val decoder: Decoder[ProjectFilter] = deriveDecoder[ProjectFilter]
    implicit val tableFilter: TableFilter[Project, ProjectFilter] =
      TableFilter.derive[Project, ProjectFilter]()
  }

  final case class ProjectModifier(
      id: Option[LongModifier],
      title: Option[StringModifier],
      description: Option[StringModifierOption],
      createdAt: Option[TimestampModifier],
      updatedAt: Option[TimestampModifier]
  )

  object ProjectModifier {
    implicit val decoder: Decoder[ProjectModifier] = deriveDecoder[ProjectModifier]
    implicit val tableModifier: TableModifier[Project, ProjectModifier] =
      TableModifier.derive[Project, ProjectModifier]()
  }

  // developer_project entity

  final case class DeveloperProjectLink(
      developerId: Long,
      projectId: Long
  )

  object DeveloperProjectLink {
    implicit val decoder: Decoder[DeveloperProjectLink] = deriveDecoder[DeveloperProjectLink]
    implicit val table: Table[DeveloperProjectLink] = Table.derive[DeveloperProjectLink, DeveloperProjectLinkKey]()
  }

  final case class DeveloperProjectLinkKey(developerId: Long, projectId: Long)

  object DeveloperProjectLinkKey {
    implicit val decoder: Decoder[DeveloperProjectLinkKey] = deriveDecoder[DeveloperProjectLinkKey]
  }

  final case class DeveloperProjectLinkFilter(
      developerId: Option[LongFilter],
      projectId: Option[LongFilter],
      AND: Option[Seq[DeveloperProjectLinkFilter]],
      OR: Option[Seq[DeveloperProjectLinkFilter]],
      NOT: Option[DeveloperProjectLinkFilter]
  ) extends FilterWrapper[DeveloperProjectLinkFilter]

  object DeveloperProjectLinkFilter {
    implicit val decoder: Decoder[DeveloperProjectLinkFilter] = deriveDecoder[DeveloperProjectLinkFilter]
    implicit val tableFilter: TableFilter[DeveloperProjectLink, DeveloperProjectLinkFilter] =
      TableFilter.derive[DeveloperProjectLink, DeveloperProjectLinkFilter]()
  }

  final case class DeveloperProjectLinkModifier(
      developerId: Option[LongModifier],
      projectId: Option[LongModifier]
  )

  object DeveloperProjectLinkModifier {
    implicit val decoder: Decoder[DeveloperProjectLinkModifier] = deriveDecoder[DeveloperProjectLinkModifier]
    implicit val tableModifier: TableModifier[DeveloperProjectLink, DeveloperProjectLinkModifier] =
      TableModifier.derive[DeveloperProjectLink, DeveloperProjectLinkModifier]()
  }
}
