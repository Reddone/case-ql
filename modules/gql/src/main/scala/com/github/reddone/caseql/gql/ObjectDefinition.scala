package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.model.gql._
import sangria.macros.derive._
import sangria.schema._

import scala.reflect.runtime.universe.{Symbol => _, _}

object ObjectDefinition {

  implicit def listContainerType[A: TypeTag, Ctx](
      implicit objectType: ObjectType[Ctx, A]
  ): ObjectType[Ctx, ListContainer[A]] =
    ObjectType[Ctx, ListContainer[A]](
      s"${typeOf[A].typeSymbol.name.toString}ListContainer",
      s"ListContainer for ${typeOf[A].typeSymbol.name.toString}",
      () =>
        List(
          Field("content", ListType(objectType), resolve = _.value.content),
          Field("pageInfo", PageInfoType, resolve = _.value.pageInfo)
        )
    )

  implicit val PageInfoType: ObjectType[Unit, PageInfo] =
    deriveObjectType[Unit, PageInfo](
      ObjectTypeName("PageInfo"),
      ObjectTypeDescription("Contains pagination info")
    )
}
