package com.github.reddone.caseql.sql.entity

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.FilterWrapper
import com.github.reddone.caseql.sql.generic.Table
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

  final case class DeveloperKey(id: Long)

  object Developer {
    implicit val decoder: Decoder[Developer]           = deriveDecoder[Developer]
    implicit val table: Table[Developer, DeveloperKey] = Table.derive[Developer, DeveloperKey]()
  }

  final case class DeveloperFilter(
      )
      extends FilterWrapper[DeveloperFilter]

  // project entity, filter, modifier

  final case class Project(
      id: Long,
      title: String,
      description: String,
      createdAt: Timestamp,
      updatedAt: Timestamp
  )

  final case class ProjectKey(id: Long)

  object Project {
    implicit val decoder: Decoder[Project]         = deriveDecoder[Project]
    implicit val table: Table[Project, ProjectKey] = Table.derive[Project, ProjectKey]()
  }

  final case class DeveloperFilter(
      )
      extends FilterWrapper[DeveloperFilter]

  // developer_project entity

  final case class DeveloperProjectLink(
      developerId: Long,
      projectId: Long
  )

  final case class DeveloperProjectLinkKey(developerId: Long, projectId: Long)

  object DeveloperProjectLink {
    implicit val decoder: Decoder[DeveloperProjectLink] =
      deriveDecoder[DeveloperProjectLink]
    implicit val table: Table[DeveloperProjectLink, DeveloperProjectLinkKey] =
      Table.derive[DeveloperProjectLink, DeveloperProjectLinkKey]()
  }

  final case class DeveloperProjectLinkFilter(
      )
      extends FilterWrapper[DeveloperProjectLinkFilter]
}
