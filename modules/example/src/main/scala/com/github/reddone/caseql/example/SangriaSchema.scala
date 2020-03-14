package com.github.reddone.caseql.example

import sangria.schema.{Field, ObjectType, Schema, StringType, fields}

trait SangriaSchema {

  val Query: ObjectType[SangriaContext, Unit] =
    ObjectType[SangriaContext, Unit]("Query", commonQueryFields[SangriaContext])

  val Mutation: ObjectType[SangriaContext, Unit] =
    ObjectType[SangriaContext, Unit]("Mutation", Nil)

  private def commonQueryFields[Ctx]: List[Field[Ctx, Unit]] =
    fields[Ctx, Unit](
      Field(
        "liveness",
        StringType,
        Some("Liveness probe"),
        arguments = Nil,
        resolve = _ => "Hey there!"
      )
    )
}
