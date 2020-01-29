package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.generic.TableFunction.extractModifier
import com.github.reddone.caseql.sql.modifier.models.Modifier
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableModifier[T, U] {
  def keys(): List[Symbol]
  def values(u: U): List[Option[Modifier[_]]]
}

object TableModifier {

  def apply[T, U](implicit ev: TableModifier[T, U]): TableModifier[T, U] = ev

  object derive {

    def apply[T, U] = new Partial[T, U]

    class Partial[T, U] {

      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
          implicit
          lgenT: LabelledGeneric.Aux[T, L],
          lgenU: LabelledGeneric.Aux[U, R],
          tableModifierLR: Lazy[ReprTableModifier.Aux[L, R, RKeys, RValues]],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Modifier[_]]]
      ): TableModifier[T, U] = new TableModifier[T, U] {
        override def keys(): List[Symbol]                    = tableModifierLR.value.keys().toList
        override def values(u: U): List[Option[Modifier[_]]] = tableModifierLR.value.values(lgenU.to(u)).toList
      }
    }
  }
}

trait ReprTableModifier[L <: HList, R <: HList] {
  type Keys <: HList
  type Values <: HList

  def keys(): Keys
  def values(r: R): Values
}

object ReprTableModifier {
  type OptionModifier[A] = Option[Modifier[A]]

  type Aux[L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] = ReprTableModifier[L, R] {
    type Keys   = Keys0
    type Values = Values0
  }

  implicit def tableModifier[
      L <: HList,
      LKeys <: HList,
      LValues <: HList,
      LValuesWrapped <: HList,
      LZipped <: HList,
      R <: HList,
      RModifier <: HList,
      RModifierKeys <: HList,
      RModifierValues <: HList,
      RAligned <: HList
  ](
      implicit
      keysL: ops.record.Keys.Aux[L, LKeys],
      valuesL: ops.record.Values.Aux[L, LValues],
      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionModifier, LValuesWrapped],
      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
      extractModifierR: ops.hlist.FlatMapper.Aux[extractModifier.type, R, RModifier],
      unzippedR: ops.record.UnzipFields.Aux[RModifier, RModifierKeys, RModifierValues],
      alignR: ops.record.AlignByKeys.Aux[RModifier, LKeys, RAligned],
      extTR: <:<[RAligned, LZipped]
  ): Aux[L, R, RModifierKeys, RModifierValues] = new ReprTableModifier[L, R] {
    override type Keys   = RModifierKeys
    override type Values = RModifierValues

    override def keys(): RModifierKeys         = unzippedR.keys()
    override def values(r: R): RModifierValues = unzippedR.values(r.flatMap(extractModifier))
  }
}
