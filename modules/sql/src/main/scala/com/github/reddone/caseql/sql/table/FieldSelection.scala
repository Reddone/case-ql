package com.github.reddone.caseql.sql.table

import shapeless.{HList, ops}

trait FieldSelection[ReprA <: HList, ReprK <: HList] {
  type Values <: HList

  def fields(kRepr: ReprK): List[String]
}

object FieldSelection {

  type Aux[ReprA <: HList, ReprK <: HList, Values0 <: HList] = FieldSelection[ReprA, ReprK] {
    type Values = Values0
  }

  implicit def derive[ReprA <: HList, ReprK <: HList, SelectedAK <: HList](
      implicit
      selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, SelectedAK],
      toListK: ops.hlist.ToList[ReprK, Symbol]
  ): Aux[ReprA, ReprK, SelectedAK] = new FieldSelection[ReprA, ReprK] {
    override type Values = SelectedAK

    override def fields(kRepr: ReprK): List[String] = kRepr.toList.map(_.name)
  }
}
