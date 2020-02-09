package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.query.Query._
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait SelectBuilderState
sealed trait SelectHasTable  extends SelectBuilderState
sealed trait SelectHasFilter extends SelectBuilderState
sealed trait SelectHasKey    extends SelectBuilderState

private[table] class SelectBuilder[S, T, K](
    table: Table[T, K],
    alias: Option[String]
) { self =>

  private[this] var fragment: Fragment = {
    val syntax         = table.internalSyntax // TODO: use alias to change syntax
    val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")
    selectFragment
  }

  private val syntax: TableSyntax[T] = table.internalSyntax

  def withFilter[FT <: EntityFilter[FT]](filter: FT)(
      implicit
      ev: S =:= SelectHasTable,
      tableFilter: TableFilter[T, FT]
  ): SelectBuilder[S with SelectHasFilter, T, K] = {
    val whereFragment = table
      .byFilterFragment(filter, alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasFilter, T, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= SelectHasTable
  ): SelectBuilder[S with SelectHasKey, T, K] = {
    val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasKey, T, K]]
  }

  def buildSelect(
      implicit ev: S =:= SelectHasTable with SelectHasFilter
  ): SQLStreamingAction[T] =
    new SQLStreamingAction[T] {
      override def toFragment: Fragment             = fragment
      override def execute: Stream[ConnectionIO, T] = fragment.query[T].stream
    }

  def buildSelectByKey(
      implicit ev: S =:= SelectHasTable with SelectHasKey
  ): SQLAction[Option[T]] =
    new SQLAction[Option[T]] {
      override def toFragment: Fragment             = fragment
      override def execute: ConnectionIO[Option[T]] = fragment.query[T].option
    }
}

private[table] object SelectBuilder {

  def forTable[T, K](table: Table[T, K], alias: Option[String]) = new SelectBuilder[SelectHasTable, T, K](table, alias)
}
