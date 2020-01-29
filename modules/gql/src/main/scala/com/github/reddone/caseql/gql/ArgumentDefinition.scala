package com.github.reddone.caseql.gql

import sangria.schema._

object ArgumentDefinition {

  val IntIdArg: Argument[Int]       = Argument("id", IntType, "Id of IntIdentifiable")
  val LongIdArg: Argument[Long]     = Argument("id", LongType, "Id of LongIdentifiable")
  val StringIdArg: Argument[String] = Argument("id", StringType, "Id of StringIdentifiable")

  val OffsetArg: Argument[Option[Int]] = Argument("offset", OptionInputType(IntType), "Offset")
  val LimitArg: Argument[Option[Int]]  = Argument("limit", OptionInputType(IntType), "Limit")
}
