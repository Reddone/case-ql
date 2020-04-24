package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.model.gql._
import sangria.macros.derive._
import sangria.schema._

import scala.reflect.runtime.universe.{Symbol => _, _}

object ObjectDefinition {

  implicit def listContainerType[A: TypeTag](
      implicit outputType: OutputType[A]
  ): ObjectType[Unit, ListContainer[A]] =
    ObjectType[Unit, ListContainer[A]](
      s"${typeOf[A].typeSymbol.name.toString}ListContainer",
      s"ListContainer for ${typeOf[A].typeSymbol.name.toString}",
      () =>
        fields[Unit, ListContainer[A]](
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
