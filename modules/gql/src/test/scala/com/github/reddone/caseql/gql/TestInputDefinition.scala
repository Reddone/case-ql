package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.InputDefinition._
import com.github.reddone.caseql.sql.ItTestModel._
import sangria.macros.derive._
import sangria.schema.InputObjectType

object TestInputDefinition {

  implicit val DeveloperFilterType: InputObjectType[DeveloperFilter] =
    deriveInputObjectType[DeveloperFilter](
      InputObjectTypeName("DeveloperFilter"),
      InputObjectTypeDescription("Filter for Developer")
    )

  implicit val ProjectFilterType: InputObjectType[ProjectFilter] =
    deriveInputObjectType[ProjectFilter](
      InputObjectTypeName("ProjectFilter"),
      InputObjectTypeDescription("Filter for Project")
    )

  implicit val DeveloperProjectLinkFilterType: InputObjectType[DeveloperProjectLinkFilter] =
    deriveInputObjectType[DeveloperProjectLinkFilter](
      InputObjectTypeName("DeveloperProjectLinkFilter"),
      InputObjectTypeDescription("Filter for DeveloperProjectLink")
    )

  implicit val TaskFilterType: InputObjectType[TaskFilter] =
    deriveInputObjectType[TaskFilter](
      InputObjectTypeName("TaskFilter"),
      InputObjectTypeDescription("Filter for Task")
    )

  implicit val DeveloperModifierType: InputObjectType[DeveloperModifier] =
    deriveInputObjectType[DeveloperModifier](
      InputObjectTypeName("DeveloperModifier"),
      InputObjectTypeDescription("Modifier for Developer")
    )

  implicit val ProjectModifierType: InputObjectType[ProjectModifier] =
    deriveInputObjectType[ProjectModifier](
      InputObjectTypeName("ProjectModifier"),
      InputObjectTypeDescription("Modifier for Project")
    )

  implicit val DeveloperProjectLinkModifierType: InputObjectType[DeveloperProjectLinkModifier] =
    deriveInputObjectType[DeveloperProjectLinkModifier](
      InputObjectTypeName("DeveloperProjectLinkModifier"),
      InputObjectTypeDescription("Modifier for DeveloperProjectLink")
    )

  implicit val TaskModifierType: InputObjectType[TaskModifier] =
    deriveInputObjectType[TaskModifier](
      InputObjectTypeName("TaskModifier"),
      InputObjectTypeDescription("Modifier for Task")
    )
}
