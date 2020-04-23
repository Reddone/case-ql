package com.github.reddone.caseql.gql

import com.github.reddone.caseql.circe.filter.decoders._
import com.github.reddone.caseql.circe.modifier.decoders._
import com.github.reddone.caseql.circe.util.decoders._
import com.github.reddone.caseql.sql.ItTestModel._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object ItTestDecoders {

  // DEVELOPER

  implicit val developerDecoder: Decoder[Developer] =
    deriveDecoder[Developer]

  implicit val developerKeyDecoder: Decoder[DeveloperKey] =
    deriveDecoder[DeveloperKey]

  implicit val developerFilterDecoder: Decoder[DeveloperFilter] =
    deriveDecoder[DeveloperFilter]

  implicit val developerModifierDecoder: Decoder[DeveloperModifier] =
    deriveDecoder[DeveloperModifier]

  // PROJECT

  implicit val projectDecoder: Decoder[Project] =
    deriveDecoder[Project]

  implicit val projectKeyDecoder: Decoder[ProjectKey] =
    deriveDecoder[ProjectKey]

  implicit val projectFilterDecoder: Decoder[ProjectFilter] =
    deriveDecoder[ProjectFilter]

  implicit val projectModifierDecoder: Decoder[ProjectModifier] =
    deriveDecoder[ProjectModifier]

  // DEVELOPER_PROJECT_LINK

  implicit val developerProjectLinkDecoder: Decoder[DeveloperProjectLink] =
    deriveDecoder[DeveloperProjectLink]

  implicit val developerProjectLinkKeyDecoder: Decoder[DeveloperProjectLinkKey] =
    deriveDecoder[DeveloperProjectLinkKey]

  implicit val developerProjectLinkFilterDecoder: Decoder[DeveloperProjectLinkFilter] =
    deriveDecoder[DeveloperProjectLinkFilter]

  implicit val developerProjectLinkModifierDecoder: Decoder[DeveloperProjectLinkModifier] =
    deriveDecoder[DeveloperProjectLinkModifier]

  // TASK

  implicit val taskDecoder: Decoder[Task] =
    deriveDecoder[Task]

  implicit val taskKeyDecoder: Decoder[TaskKey] =
    deriveDecoder[TaskKey]

  implicit val taskFilterDecoder: Decoder[TaskFilter] =
    deriveDecoder[TaskFilter]

  implicit val taskModifierDecoder: Decoder[TaskModifier] =
    deriveDecoder[TaskModifier]
}
