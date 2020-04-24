package com.github.reddone.caseql.example.schema

import com.github.reddone.caseql.example.model.decoders._
import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.example.schema.InputDefinition._
import sangria.schema._
import sangria.marshalling.circe._

object ArgumentDefinition {

  // KEY

  val DeveloperKeyArg: Argument[DeveloperKey] =
    Argument("key", DeveloperKeyType)
  val ProjectKeyArg: Argument[ProjectKey] =
    Argument("key", ProjectKeyType)
  val DeveloperProjectLinkKeyArg: Argument[DeveloperProjectLinkKey] =
    Argument("key", DeveloperProjectLinkKeyType)
  val TaskKeyArg: Argument[TaskKey] =
    Argument("key", TaskKeyType)

  // FILTER

  val DeveloperFilterArg: Argument[DeveloperFilter] =
    Argument("filter", DeveloperFilterType)
  val ProjectFilterArg: Argument[ProjectFilter] =
    Argument("filter", ProjectFilterType)
  val DeveloperProjectLinkFilterArg: Argument[DeveloperProjectLinkFilter] =
    Argument("filter", DeveloperProjectLinkFilterType)
  val TaskFilterArg: Argument[TaskFilter] =
    Argument("filter", TaskFilterType)

  // MODIFIER

  val DeveloperModifierArg: Argument[DeveloperModifier] =
    Argument("modifier", DeveloperModifierType)
  val ProjectModifierArg: Argument[ProjectModifier] =
    Argument("modifier", ProjectModifierType)
  val DeveloperProjectLinkModifierArg: Argument[DeveloperProjectLinkModifier] =
    Argument("modifier", DeveloperProjectLinkModifierType)
  val TaskModifierArg: Argument[TaskModifier] =
    Argument("modifier", TaskModifierType)

  // COMMON

  val IntIdArg: Argument[Int]       = Argument("id", IntType, "Int id")
  val LongIdArg: Argument[Long]     = Argument("id", LongType, "Long id")
  val StringIdArg: Argument[String] = Argument("id", StringType, "String id")

  val OffsetArg: Argument[Option[Int]] = Argument("offset", OptionInputType(IntType), "Offset")
  val LimitArg: Argument[Option[Int]]  = Argument("limit", OptionInputType(IntType), "Limit")
}
