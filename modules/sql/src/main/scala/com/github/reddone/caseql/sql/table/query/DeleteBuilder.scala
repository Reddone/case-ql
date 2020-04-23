package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait DeleteBuilderState
sealed trait DeleteHasTable  extends DeleteBuilderState
sealed trait DeleteHasFilter extends DeleteBuilderState
sealed trait DeleteHasKey    extends DeleteBuilderState

sealed abstract class DeleteBuilder[S, A, K](
    table: Table[A, K]
) extends QueryBuilder[A, K](table, None) { self =>

  private[this] var fragment: Fragment = const(
    s"$Delete $From ${querySyntax.name}"
  )

  def withFilter[FA <: EntityFilter[FA]](filter: FA)(
      implicit
      ev: S =:= DeleteHasTable,
      tableFilter: TableFilter[A, FA]
  ): DeleteBuilder[S with DeleteHasFilter, A, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, None)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[DeleteBuilder[S with DeleteHasFilter, A, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= DeleteHasTable
  ): DeleteBuilder[S with DeleteHasKey, A, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[DeleteBuilder[S with DeleteHasKey, A, K]]
  }

  def buildDelete(
      implicit ev: S =:= DeleteHasTable with DeleteHasFilter
  ): SqlAction[Int] = new SqlAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildDeleteReturningKeys(
      implicit ev: S =:= DeleteHasTable with DeleteHasFilter
  ): SqlStreamingAction[K] = new SqlStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }

  def buildDeleteByKey(
      implicit ev: S =:= DeleteHasTable with DeleteHasKey
  ): SqlAction[Int] = new SqlAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildDeleteByKeyReturningKeys(
      implicit ev: S =:= DeleteHasTable with DeleteHasKey
  ): SqlStreamingAction[K] = new SqlStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }
}

object DeleteBuilder {

  def apply[A, K](implicit table: Table[A, K]): DeleteBuilder[DeleteHasTable, A, K] =
    new DeleteBuilder[DeleteHasTable, A, K](table) {}

  def forTable[A, K](table: Table[A, K]): DeleteBuilder[DeleteHasTable, A, K] =
    new DeleteBuilder[DeleteHasTable, A, K](table) {}
}
