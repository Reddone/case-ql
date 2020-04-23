package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.TestDecoders._
import com.github.reddone.caseql.gql.TestInputDefinition._
import com.github.reddone.caseql.sql.TestModel._
import sangria.schema.Argument
import sangria.marshalling.circe._

object TestArgumentDefinition {

  // TEST

  val TestFilterArg: Argument[TestFilter] =
    Argument("filter", TestFilterType)
  val TestModifierArg: Argument[TestModifier] =
    Argument("modifier", TestModifierType)

  // RELATION FILTER TEST

  val TestLeftFilterArg: Argument[TestLeftFilter] =
    Argument("filter", TestLeftFilterType)
  val TestDirectFilterArg: Argument[TestDirectFilter] =
    Argument("filter", TestDirectFilterType)
  val TestRightFilterArg: Argument[TestRightFilter] =
    Argument("filter", TestRightFilterType)
  val TestJunctionFilterArg: Argument[TestJunctionFilter] =
    Argument("filter", TestJunctionFilterType)
}
