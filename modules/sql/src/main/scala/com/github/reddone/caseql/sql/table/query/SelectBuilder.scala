package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableSyntax}
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
) extends QueryBuilder[A, K](table) { self =>

  final val querySyntax: TableSyntax[A] = alias.fold(table.syntax)(table.syntax.withAlias)

  private[this] var fragment: Fragment = const(
    s"$Select ${querySyntax.selectionColumns.mkString(", ")} " +
      s"$From ${querySyntax.fullName}"
  )

  def withFilter[FA <: EntityFilter[FA]](filter: FA)(
      implicit
      ev: S =:= SelectHasTable,
      tableFilter: TableFilter[A, FA]
  ): SelectBuilder[S with SelectHasFilter, A, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, querySyntax.alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasFilter, A, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= SelectHasTable
  ): SelectBuilder[S with SelectHasKey, A, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key, querySyntax.alias)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasKey, A, K]]
  }

  def buildSelect(
      implicit ev: S =:= SelectHasTable with SelectHasFilter
  ): SqlStreamingAction[A] =
    new SqlStreamingAction[A] {
      override def toFragment: Fragment             = fragment
      override def execute: Stream[ConnectionIO, A] = fragment.query[A](table.read).stream
    }

  def buildSelectByKey(
      implicit ev: S =:= SelectHasTable with SelectHasKey
  ): SqlAction[Option[A]] =
    new SqlAction[Option[A]] {
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
