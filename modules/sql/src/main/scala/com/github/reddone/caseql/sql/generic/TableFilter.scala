package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.filter.FilterWrapper
import com.github.reddone.caseql.sql.generic.TableFunction.extractFilter
import com.github.reddone.caseql.sql.filter.models.Filter
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableFilter[T, U <: FilterWrapper[U]] {
  def keys(): List[Symbol]
  def values(u: U): List[Option[Filter[_]]]
}

object TableFilter {

  def apply[T, U <: FilterWrapper[U]](implicit ev: TableFilter[T, U]): TableFilter[T, U] = ev

  object derive {

    def apply[T, U <: FilterWrapper[U]] = new Partial[T, U]

    class Partial[T, U <: FilterWrapper[U]] {

      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
          implicit
          lgenT: LabelledGeneric.Aux[T, L],
          lgenU: LabelledGeneric.Aux[U, R],
          tableFilterLR: Lazy[ReprTableFilter.Aux[L, R, RKeys, RValues]],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
      ): TableFilter[T, U] = new TableFilter[T, U] {
        override def keys(): List[Symbol]                  = tableFilterLR.value.keys().toList
        override def values(u: U): List[Option[Filter[_]]] = tableFilterLR.value.values(lgenU.to(u)).toList
      }
    }
  }
}

trait ReprTableFilter[L <: HList, R <: HList] {
  type Keys <: HList
  type Values <: HList

  def keys(): Keys
  def values(r: R): Values
}

object ReprTableFilter {
  type OptionFilter[A] = Option[Filter[A]]

  type Aux[L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] = ReprTableFilter[L, R] {
    type Keys   = Keys0
    type Values = Values0
  }

  implicit def tableFilter[
      L <: HList,
      LKeys <: HList,
      LValues <: HList,
      LValuesWrapped <: HList,
      LZipped <: HList,
      R <: HList,
      RFilter <: HList,
      RFilterKeys <: HList,
      RFilterValues <: HList,
      RAligned <: HList
  ](
      implicit
      keysL: ops.record.Keys.Aux[L, LKeys],
      valuesL: ops.record.Values.Aux[L, LValues],
      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionFilter, LValuesWrapped],
      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
      extractFilterR: ops.hlist.FlatMapper.Aux[extractFilter.type, R, RFilter],
      unzippedR: ops.record.UnzipFields.Aux[RFilter, RFilterKeys, RFilterValues],
      alignR: ops.record.AlignByKeys.Aux[RFilter, LKeys, RAligned],
      extTR: <:<[RAligned, LZipped]
  ): Aux[L, R, RFilterKeys, RFilterValues] = new ReprTableFilter[L, R] {
    override type Keys   = RFilterKeys
    override type Values = RFilterValues

    override def keys(): RFilterKeys         = unzippedR.keys()
    override def values(r: R): RFilterValues = unzippedR.values(r.flatMap(extractFilter))
  }
}
