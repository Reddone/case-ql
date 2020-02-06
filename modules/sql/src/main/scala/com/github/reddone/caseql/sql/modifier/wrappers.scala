package com.github.reddone.caseql.sql.modifier

object wrappers {

  trait EntityModifier[MA <: EntityModifier[MA]] {}
}
