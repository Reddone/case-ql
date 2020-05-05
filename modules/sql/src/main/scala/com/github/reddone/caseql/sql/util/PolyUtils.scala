package com.github.reddone.caseql.sql.util

import shapeless.{::, HNil, Poly1, Witness}
import shapeless.labelled.FieldType

object PolyUtils {

  trait extract[T] extends skip {
    implicit def atType[K <: Symbol, V <: T](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, V] :: HNil] =
      at[FieldType[K, V]](_ :: HNil)
  }

  trait skip extends Poly1 {
    implicit def atDefault[T]: Case.Aux[T, HNil] = at[T](_ => HNil)
  }
}
