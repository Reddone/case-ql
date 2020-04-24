package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.TestDecoders._
import com.github.reddone.caseql.gql.TestInputDefinition._
import com.github.reddone.caseql.sql.TestModel._
import sangria.schema.Argument
import sangria.marshalling.circe._

object TestArgumentDefinition {

  // KEY

  val TestKeyArg: Argument[TestKey] =
    Argument("key", TestKeyType)
  val TestLeftKeyArg: Argument[TestLeftKey] =
    Argument("key", TestLeftKeyType)
  val TestDirectKeyArg: Argument[TestDirectKey] =
    Argument("key", TestDirectKeyType)
  val TestRightKeyArg: Argument[TestRightKey] =
    Argument("key", TestRightKeyType)
  val TestJunctionKeyArg: Argument[TestJunctionKey] =
    Argument("key", TestJunctionKeyType)

  // FILTER

  val TestFilterArg: Argument[TestFilter] =
    Argument("filter", TestFilterType)
  val TestLeftFilterArg: Argument[TestLeftFilter] =
    Argument("filter", TestLeftFilterType)
  val TestDirectFilterArg: Argument[TestDirectFilter] =
    Argument("filter", TestDirectFilterType)
  val TestRightFilterArg: Argument[TestRightFilter] =
    Argument("filter", TestRightFilterType)
  val TestJunctionFilterArg: Argument[TestJunctionFilter] =
    Argument("filter", TestJunctionFilterType)

  // MODIFIER

  val TestModifierArg: Argument[TestModifier] =
    Argument("modifier", TestModifierType)
}
