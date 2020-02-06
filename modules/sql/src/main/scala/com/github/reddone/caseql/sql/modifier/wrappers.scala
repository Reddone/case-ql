package com.github.reddone.caseql.sql.modifier

object wrappers {

  trait EntityModifier[A, MA <: EntityModifier[A, MA]] {}
}
