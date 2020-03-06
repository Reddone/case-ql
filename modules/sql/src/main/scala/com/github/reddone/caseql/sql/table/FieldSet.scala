package com.github.reddone.caseql.sql.table

import shapeless.{HList, LUBConstraint, Poly1, SingletonProductArgs, ops, tag}
import shapeless.tag.@@

object toTaggedSymbol extends Poly1 {

  implicit def atString[K <: String]: Case.Aux[K, Symbol @@ K] =
    at[K] { k =>
      new tag.Tagger[K].apply(Symbol(k))
    }
}

object FieldSet extends SingletonProductArgs {

  def applyProduct[L <: HList, Mapped <: HList](l: L)(
      implicit
      lub: LUBConstraint[L, String],
      taggedSymbol: ops.hlist.Mapper.Aux[toTaggedSymbol.type, L, Mapped]
  ): Mapped = l.map(toTaggedSymbol)
}
