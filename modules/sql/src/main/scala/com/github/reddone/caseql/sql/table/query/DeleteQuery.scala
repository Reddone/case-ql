package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.query.Query._
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait DeleteBuilderState
sealed trait DeleteHasTable  extends DeleteBuilderState
sealed trait DeleteHasFilter extends DeleteBuilderState
sealed trait DeleteHasKey    extends DeleteBuilderState

private[table] class DeleteBuilder[S, T, K](
    table: Table[T, K],
    alias: Option[String]
) { self =>

  private[this] var fragment: Fragment = {
    val name           = table.internalSyntax.name // TODO: use alias
    val deleteFragment = const(s"$Delete $From $name")
    deleteFragment
  }

  private val syntax: TableSyntax[T] = table.internalSyntax

  def withFilter[FT <: EntityFilter[FT]](filter: FT)(
      implicit
      ev: S =:= DeleteHasTable,
      tableFilter: TableFilter[T, FT]
  ): DeleteBuilder[S with DeleteHasFilter, T, K] = {
    val whereFragment = table
      .byFilterFragment(filter, alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[DeleteBuilder[S with DeleteHasFilter, T, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= DeleteHasTable
  ): DeleteBuilder[S with DeleteHasKey, T, K] = {
    val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[DeleteBuilder[S with DeleteHasKey, T, K]]
  }

  def buildDelete(
      implicit ev: S =:= DeleteHasTable with DeleteHasFilter
  ): SQLAction[Int] = new SQLAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildDeleteReturningKeys(
      implicit ev: S =:= DeleteHasTable with DeleteHasFilter
  ): SQLStreamingAction[K] = new SQLStreamingAction[K] {
    override def toFragment: Fragment             = fragment
    override def execute: Stream[ConnectionIO, K] = fragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
  }

  def buildDeleteByKey(
      implicit ev: S =:= DeleteHasTable with DeleteHasKey
  ): SQLAction[Int] = new SQLAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildDeleteByKeyReturningKeys(
      implicit ev: S =:= DeleteHasTable with DeleteHasKey
  ): SQLStreamingAction[K] = new SQLStreamingAction[K] {
    override def toFragment: Fragment             = fragment
    override def execute: Stream[ConnectionIO, K] = fragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
  }
}

private[table] object DeleteBuilder {

  def forTable[T, K](table: Table[T, K], alias: Option[String]) = new DeleteBuilder[DeleteHasTable, T, K](table, alias)
}
