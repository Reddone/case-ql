package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait SelectBuilderState
sealed trait SelectHasTable  extends SelectBuilderState
sealed trait SelectHasFilter extends SelectBuilderState
sealed trait SelectHasKey    extends SelectBuilderState

sealed abstract class SelectBuilder[S, A, K](
    table: Table[A, K],
    alias: Option[String]
) extends QueryBuilder[A, K](table, alias) { self =>

  private[this] var fragment: Fragment = const(
    s"$Select ${querySyntax.aliasedColumns.mkString(", ")} $From ${querySyntax.aliasedName}"
  )

  def withFilter[FA <: EntityFilter[FA]](filter: FA)(
      implicit
      ev: S =:= SelectHasTable,
      tableFilter: TableFilter[A, FA]
  ): SelectBuilder[S with SelectHasFilter, A, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasFilter, A, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= SelectHasTable
  ): SelectBuilder[S with SelectHasKey, A, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasKey, A, K]]
  }

  def buildSelect(
      implicit ev: S =:= SelectHasTable with SelectHasFilter
  ): SQLStreamingAction[A] =
    new SQLStreamingAction[A] {
      override def toFragment: Fragment             = fragment
      override def execute: Stream[ConnectionIO, A] = fragment.query[A](table.read).stream
    }

  def buildSelectByKey(
      implicit ev: S =:= SelectHasTable with SelectHasKey
  ): SQLAction[Option[A]] =
    new SQLAction[Option[A]] {
      override def toFragment: Fragment             = fragment
      override def execute: ConnectionIO[Option[A]] = fragment.query[A](table.read).option
    }
}

object SelectBuilder {

  def apply[A, K](alias: Option[String])(implicit table: Table[A, K]): SelectBuilder[SelectHasTable, A, K] =
    new SelectBuilder[SelectHasTable, A, K](table, alias) {}

  def forTable[A, K](table: Table[A, K], alias: Option[String]): SelectBuilder[SelectHasTable, A, K] =
    new SelectBuilder[SelectHasTable, A, K](table, alias) {}
}
