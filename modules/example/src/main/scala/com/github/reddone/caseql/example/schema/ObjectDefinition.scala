package com.github.reddone.caseql.example.schema

import cats.effect.Effect
import com.github.reddone.caseql.example.SangriaContext
import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.example.model.gql._
import com.github.reddone.caseql.gql.ByteTypeDefinition._
import com.github.reddone.caseql.gql.EnumDefinition._
import com.github.reddone.caseql.gql.JavaSqlTypeDefinition._
import com.github.reddone.caseql.gql.JavaTimeTypeDefinition._
import sangria.macros.derive._
import sangria.schema._

import scala.reflect.runtime.universe.{Symbol => _, _}

object ObjectDefinition {

  // DEVELOPER

  implicit val DeveloperIdType: ObjectType[Unit, DeveloperKey] =
    deriveObjectType[Unit, DeveloperKey](
      ObjectTypeName("DeveloperId"),
      ObjectTypeDescription("Developer entity id")
    )

  implicit def DeveloperType[F[_]: Effect]: ObjectType[SangriaContext[F], Developer] =
    deriveObjectType[SangriaContext[F], Developer](
      ObjectTypeName("Developer"),
      ObjectTypeDescription("Developer entity")
    )

  // PROJECT

  implicit val ProjectIdType: ObjectType[Unit, ProjectKey] =
    deriveObjectType[Unit, ProjectKey](
      ObjectTypeName("ProjectId"),
      ObjectTypeDescription("Project entity id")
    )

  implicit def ProjectType[F[_]: Effect]: ObjectType[SangriaContext[F], Project] =
    deriveObjectType[SangriaContext[F], Project](
      ObjectTypeName("Project"),
      ObjectTypeDescription("Project entity")
    )

  // DEVELOPER_PROJECT_LINK

  implicit val DeveloperProjectLinkIdType: ObjectType[Unit, DeveloperProjectLinkKey] =
    deriveObjectType[Unit, DeveloperProjectLinkKey](
      ObjectTypeName("DeveloperProjectLinkId"),
      ObjectTypeDescription("DeveloperProjectLink entity id")
    )

  implicit def DeveloperProjectLinkType[F[_]: Effect]: ObjectType[SangriaContext[F], DeveloperProjectLink] =
    deriveObjectType[SangriaContext[F], DeveloperProjectLink](
      ObjectTypeName("DeveloperProjectLink"),
      ObjectTypeDescription("DeveloperProjectLink entity")
    )

  // TASK

  implicit val TaskIdType: ObjectType[Unit, TaskKey] =
    deriveObjectType[Unit, TaskKey](
      ObjectTypeName("TaskId"),
      ObjectTypeDescription("Task entity id")
    )

  implicit def TaskType[F[_]: Effect]: ObjectType[SangriaContext[F], Task] =
    deriveObjectType[SangriaContext[F], Task](
      ObjectTypeName("Task"),
      ObjectTypeDescription("Task entity")
    )

  // COMMON

  implicit def listContainerType[Ctx, A: TypeTag](
      implicit outputType: OutputType[A]
  ): ObjectType[Ctx, ListContainer[A]] =
    ObjectType[Ctx, ListContainer[A]](
      s"${typeOf[A].typeSymbol.name.toString}ListContainer",
      s"ListContainer for ${typeOf[A].typeSymbol.name.toString}",
      () =>
        fields[Ctx, ListContainer[A]](
          Field("content", ListType(outputType), resolve = _.value.content),
          Field("pageInfo", PageInfoType, resolve = _.value.pageInfo)
        )
    )

  implicit val PageInfoType: ObjectType[Unit, PageInfo] =
    deriveObjectType[Unit, PageInfo](
      ObjectTypeName("PageInfo"),
      ObjectTypeDescription("Contains pagination info")
    )
}
