package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.model.gql._
import sangria.macros.derive._
import sangria.schema._

object ObjectDefinition {

  implicit val IntIdentifiableType: InterfaceType[Unit, IntIdentifiable] =
    InterfaceType[Unit, IntIdentifiable](
      "IntIdentifiable",
      "Entity that can be identified with an int",
      fields[Unit, IntIdentifiable](
        Field("id", IntType, Some("Identity"), resolve = _.value.id)
      )
    )

  implicit val LongIdentifiableType: InterfaceType[Unit, LongIdentifiable] =
    InterfaceType[Unit, LongIdentifiable](
      "LongIdentifiable",
      "Entity that can be identified with a long",
      fields[Unit, LongIdentifiable](
        Field("id", LongType, Some("Identity"), resolve = _.value.id)
      )
    )

  implicit val StringIdentifiableType: InterfaceType[Unit, StringIdentifiable] =
    InterfaceType[Unit, StringIdentifiable](
      "StringIdentifiable",
      "Entity that can be identified with a string",
      fields[Unit, StringIdentifiable](
        Field("id", StringType, Some("Identity"), resolve = _.value.id)
      )
    )

  implicit val PageInfoType: ObjectType[Unit, PageInfo] =
    deriveObjectType[Unit, PageInfo](
      ObjectTypeName("PageInfo"),
      ObjectTypeDescription("Contains pagination info")
    )
}
