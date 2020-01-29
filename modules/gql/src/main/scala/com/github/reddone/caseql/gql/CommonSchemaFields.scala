package com.github.reddone.caseql.gql

import sangria.schema.{Field, StringType, fields}

trait CommonSchemaFields {

  def commonQueryFields[Ctx]: List[Field[Ctx, Unit]] =
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
