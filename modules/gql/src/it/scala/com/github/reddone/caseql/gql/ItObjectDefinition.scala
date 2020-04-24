package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.ByteTypeDefinition._
import com.github.reddone.caseql.gql.EnumDefinition._
import com.github.reddone.caseql.gql.JavaSqlTypeDefinition._
import com.github.reddone.caseql.gql.JavaTimeTypeDefinition._
import com.github.reddone.caseql.sql.ItTestModel._
import sangria.macros.derive._
import sangria.schema._

object ItObjectDefinition {

  // DEVELOPER

  implicit val DeveloperType: ObjectType[Unit, Developer] =
    deriveObjectType[Unit, Developer](
      ObjectTypeName("Developer"),
      ObjectTypeDescription("Developer entity")
    )

  // PROJECT

  implicit val ProjectType: ObjectType[Unit, Project] =
    deriveObjectType[Unit, Project](
      ObjectTypeName("Project"),
      ObjectTypeDescription("Project entity")
    )

  // DEVELOPER_PROJECT_LINK

  implicit val DeveloperProjectLinkType: ObjectType[Unit, DeveloperProjectLink] =
    deriveObjectType[Unit, DeveloperProjectLink](
      ObjectTypeName("DeveloperProjectLink"),
      ObjectTypeDescription("DeveloperProjectLink entity")
    )

  // TASK

  implicit val TaskType: ObjectType[Unit, Task] =
    deriveObjectType[Unit, Task](
      ObjectTypeName("Task"),
      ObjectTypeDescription("Task entity")
    )
}
