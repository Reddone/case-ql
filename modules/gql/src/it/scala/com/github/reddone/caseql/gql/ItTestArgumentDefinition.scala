package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.ItTestDecoders._
import com.github.reddone.caseql.gql.ItTestInputDefinition._
import com.github.reddone.caseql.sql.ItTestModel._
import sangria.schema.Argument
import sangria.marshalling.circe._

object ItTestArgumentDefinition {

  val DeveloperFilterArg = Argument("filter", DeveloperFilterType)
}
