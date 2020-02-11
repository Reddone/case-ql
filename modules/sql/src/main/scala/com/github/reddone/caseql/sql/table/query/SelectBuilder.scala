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

final class SelectBuilder[S, T, K](
    table: Table[T, K],
    alias: Option[String]
) extends QueryBuilder[T, K](table, alias) { self =>

  private[this] var fragment: Fragment = const(
    s"""
       |$Select ${querySyntax.columns.mkString(", ")} 
       |$From ${if (querySyntax.alias.isEmpty) querySyntax.name else querySyntax.aliasedName}
       |""".stripMargin
  )

  def withFilter[FT <: EntityFilter[FT]](filter: FT)(
      implicit
      ev: S =:= SelectHasTable,
      tableFilter: TableFilter[T, FT]
  ): SelectBuilder[S with SelectHasFilter, T, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasFilter, T, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= SelectHasTable
  ): SelectBuilder[S with SelectHasKey, T, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key, alias)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[SelectBuilder[S with SelectHasKey, T, K]]
  }

  def buildSelect(
      implicit ev: S =:= SelectHasTable with SelectHasFilter
  ): SQLStreamingAction[T] =
    new SQLStreamingAction[T] {
      override def toFragment: Fragment             = fragment
      override def execute: Stream[ConnectionIO, T] = fragment.query[T](table.read).stream
    }

  def buildSelectByKey(
      implicit ev: S =:= SelectHasTable with SelectHasKey
  ): SQLAction[Option[T]] =
    new SQLAction[Option[T]] {
      override def toFragment: Fragment             = fragment
      override def execute: ConnectionIO[Option[T]] = fragment.query[T](table.read).option
    }
}

object SelectBuilder {

  def apply[T, K](alias: Option[String])(implicit table: Table[T, K]) =
    new SelectBuilder[SelectHasTable, T, K](table, alias)

  def forTable[T, K](table: Table[T, K], alias: Option[String]) =
    new SelectBuilder[SelectHasTable, T, K](table, alias)
}
