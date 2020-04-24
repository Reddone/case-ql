package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.ItTestDecoders._
import com.github.reddone.caseql.gql.ItTestInputDefinition._
import com.github.reddone.caseql.sql.ItTestModel._
import sangria.schema.Argument
import sangria.marshalling.circe._

object ItTestArgumentDefinition {

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
}
