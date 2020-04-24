package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.ByteTypeDefinition._
import com.github.reddone.caseql.gql.EnumDefinition._
import com.github.reddone.caseql.gql.JavaSqlTypeDefinition._
import com.github.reddone.caseql.gql.JavaTimeTypeDefinition._
import com.github.reddone.caseql.sql.ItTestModel._
import sangria.macros.derive._
import sangria.schema._

object ItTestObjectDefinition {

  // DEVELOPER

  implicit val DeveloperIdType: ObjectType[Unit, DeveloperKey] =
    deriveObjectType[Unit, DeveloperKey](
      ObjectTypeName("DeveloperId"),
      ObjectTypeDescription("Developer entity id")
    )

  implicit val DeveloperType: ObjectType[Unit, Developer] =
    deriveObjectType[Unit, Developer](
      ObjectTypeName("Developer"),
      ObjectTypeDescription("Developer entity")
    )

  // PROJECT

  implicit val ProjectIdType: ObjectType[Unit, ProjectKey] =
    deriveObjectType[Unit, ProjectKey](
      ObjectTypeName("ProjectId"),
      ObjectTypeDescription("Project entity id")
    )

  implicit val ProjectType: ObjectType[Unit, Project] =
    deriveObjectType[Unit, Project](
      ObjectTypeName("Project"),
      ObjectTypeDescription("Project entity")
    )

  // DEVELOPER_PROJECT_LINK

  implicit val DeveloperProjectLinkIdType: ObjectType[Unit, DeveloperProjectLinkKey] =
    deriveObjectType[Unit, DeveloperProjectLinkKey](
      ObjectTypeName("DeveloperProjectLinkId"),
      ObjectTypeDescription("DeveloperProjectLink entity id")
    )

  implicit val DeveloperProjectLinkType: ObjectType[Unit, DeveloperProjectLink] =
    deriveObjectType[Unit, DeveloperProjectLink](
      ObjectTypeName("DeveloperProjectLink"),
      ObjectTypeDescription("DeveloperProjectLink entity")
    )

  // TASK

  implicit val TaskIdType: ObjectType[Unit, TaskKey] =
    deriveObjectType[Unit, TaskKey](
      ObjectTypeName("TaskId"),
      ObjectTypeDescription("Task entity id")
    )

  implicit val TaskType: ObjectType[Unit, Task] =
    deriveObjectType[Unit, Task](
      ObjectTypeName("Task"),
      ObjectTypeDescription("Task entity")
    )
}
