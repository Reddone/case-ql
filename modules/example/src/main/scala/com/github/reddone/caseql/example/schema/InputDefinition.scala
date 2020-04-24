package com.github.reddone.caseql.example.schema

import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.gql.InputDefinition._
import sangria.macros.derive.{InputObjectTypeDescription, InputObjectTypeName, deriveInputObjectType}
import sangria.schema.InputObjectType

object InputDefinition {

  // KEY

  val DeveloperKeyType: InputObjectType[DeveloperKey] =
    deriveInputObjectType[DeveloperKey](
      InputObjectTypeName("DeveloperKey"),
      InputObjectTypeDescription("Key for Developer")
    )

  val ProjectKeyType: InputObjectType[ProjectKey] =
    deriveInputObjectType[ProjectKey](
      InputObjectTypeName("ProjectKey"),
      InputObjectTypeDescription("Key for Project")
    )

  val DeveloperProjectLinkKeyType: InputObjectType[DeveloperProjectLinkKey] =
    deriveInputObjectType[DeveloperProjectLinkKey](
      InputObjectTypeName("DeveloperProjectLinkKey"),
      InputObjectTypeDescription("Key for DeveloperProjectLink")
    )

  val TaskKeyType: InputObjectType[TaskKey] =
    deriveInputObjectType[TaskKey](
      InputObjectTypeName("TaskKey"),
      InputObjectTypeDescription("Key for Task")
    )

  // FILTER

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

  // MODIFIER

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
