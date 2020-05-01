package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.InputDefinition._
import com.github.reddone.caseql.sql.model.db._
import sangria.macros.derive._
import sangria.schema.InputObjectType

object TestInputDefinition {

  // FILTER

  implicit val TestFilterType: InputObjectType[TestFilter] =
    deriveInputObjectType[TestFilter](
      InputObjectTypeName("TestFilter"),
      InputObjectTypeDescription("Filter for Test")
    )

  implicit val TestLeftFilterType: InputObjectType[TestLeftFilter] =
    deriveInputObjectType[TestLeftFilter](
      InputObjectTypeName("TestLeftFilter"),
      InputObjectTypeDescription("Filter for TestLeft")
    )

  implicit val TestDirectFilterType: InputObjectType[TestDirectFilter] =
    deriveInputObjectType[TestDirectFilter](
      InputObjectTypeName("TestDirectFilter"),
      InputObjectTypeDescription("Filter for TestDirect")
    )

  implicit val TestRightFilterType: InputObjectType[TestRightFilter] =
    deriveInputObjectType[TestRightFilter](
      InputObjectTypeName("TestRightFilter"),
      InputObjectTypeDescription("Filter for TestRight")
    )

  implicit val TestJunctionFilterType: InputObjectType[TestJunctionFilter] =
    deriveInputObjectType[TestJunctionFilter](
      InputObjectTypeName("TestJunctionFilter"),
      InputObjectTypeDescription("Filter for TestJunction")
    )

  // MODIFIER

  implicit val TestModifierType: InputObjectType[TestModifier] =
    deriveInputObjectType[TestModifier](
      InputObjectTypeName("TestModifier"),
      InputObjectTypeDescription("Modifier for Test")
    )
}
