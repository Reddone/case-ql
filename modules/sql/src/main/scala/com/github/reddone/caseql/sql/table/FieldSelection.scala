package com.github.reddone.caseql.sql.table

import shapeless.{HList, ops}

trait FieldSelection[ReprA <: HList, ReprK <: HList] {
  type Values <: HList

  def fields(fieldsRepr: ReprK): List[String]
}

object FieldSelection {

  type Aux[ReprA <: HList, ReprK <: HList, Values0 <: HList] = FieldSelection[ReprA, ReprK] {
    type Values = Values0
  }

  implicit def derive[ReprA <: HList, ReprK <: HList, Selected <: HList](
      implicit
      selectAll: ops.record.SelectAll.Aux[ReprA, ReprK, Selected],
      toList: ops.hlist.ToList[ReprK, Symbol]
  ): Aux[ReprA, ReprK, Selected] = new FieldSelection[ReprA, ReprK] {
    override type Values = Selected

    override def fields(fieldsRepr: ReprK): List[String] = fieldsRepr.toList.map(_.name)
  }
}
