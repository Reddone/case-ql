package com.github.reddone.caseql.example.schema

import sangria.schema._

object ArgumentDefinition {

  val IntIdArg: Argument[Int]       = Argument("id", IntType, "Int id")
  val LongIdArg: Argument[Long]     = Argument("id", LongType, "Long id")
  val StringIdArg: Argument[String] = Argument("id", StringType, "String id")

  val OffsetArg: Argument[Option[Int]] = Argument("offset", OptionInputType(IntType), "Offset")
  val LimitArg: Argument[Option[Int]]  = Argument("limit", OptionInputType(IntType), "Limit")
}
