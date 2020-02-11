package com.github.reddone.caseql.gql

import com.github.reddone.caseql.sql.modifier.models.{ModifierAction, ModifierOptionAction}
import sangria.schema.{EnumType, EnumValue}

object EnumDefinition {

  // action field names
  private val DefaultName = "DEFAULT"
  private val SetName     = "SET"
  private val NullName    = "NULL"

  implicit val ModifierActionType: EnumType[ModifierAction.Value] =
    EnumType(
      "ModifierAction",
      Some("Modifier action"),
      List(
        EnumValue(DefaultName, value = ModifierAction.Default),
        EnumValue(SetName, value = ModifierAction.Set)
      )
    )

  implicit val ModifierOptionActionType: EnumType[ModifierOptionAction.Value] =
    EnumType(
      "ModifierOptionAction",
      Some("Modifier option action"),
      List(
        EnumValue(DefaultName, value = ModifierOptionAction.Default),
        EnumValue(SetName, value = ModifierOptionAction.Set),
        EnumValue(NullName, value = ModifierOptionAction.Null)
      )
    )
}
