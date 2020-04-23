package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableModifier}
import com.github.reddone.caseql.sql.tokens.{Update => UpdateToken, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait UpdateBuilderState
sealed trait UpdateHasTable    extends UpdateBuilderState
sealed trait UpdateHasModifier extends UpdateBuilderState
sealed trait UpdateHasFilter   extends UpdateBuilderState
sealed trait UpdateHasKey      extends UpdateBuilderState

sealed abstract class UpdateBuilder[S <: UpdateBuilderState, A, K](
    table: Table[A, K]
) extends QueryBuilder[A, K](table, None) { self =>

  private[this] var fragment: Fragment = const(
    s"$UpdateToken ${querySyntax.name}"
  )

  def withModifier[MA <: EntityModifier[MA]](modifier: MA)(
      implicit
      ev: S =:= UpdateHasTable,
      tableModifier: TableModifier[A, MA]
  ): UpdateBuilder[S with UpdateHasModifier, A, K] = {
    val namedFragments = tableModifier
      .primitiveModifierNamedFragments(modifier)
      .filter(_._2.nonEmpty)
      .map {
        case (column, modifier) => (column, modifier.get)
      }
    val setFragment = Fragments.set(namedFragments.map {
      case (col, param) => const(col + " =") ++ param
    }: _*) // love scala emojis
    fragment = fragment ++ setFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasModifier, A, K]]
  }

  def withFilter[FA <: EntityFilter[FA]](filter: FA)(
      implicit
      ev: S =:= UpdateHasTable with UpdateHasModifier,
      tableFilter: TableFilter[A, FA]
  ): UpdateBuilder[S with UpdateHasFilter, A, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, None)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasFilter, A, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier
  ): UpdateBuilder[S with UpdateHasKey, A, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasKey, A, K]]
  }

  def buildUpdate(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasFilter
  ): SqlAction[Int] = new SqlAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildUpdateReturningKeys(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasFilter
  ): SqlStreamingAction[K] = new SqlStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }

  def buildUpdateByKey(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasKey
  ): SqlAction[Int] = new SqlAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildUpdateByKeyReturningKeys(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasKey
  ): SqlStreamingAction[K] = new SqlStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }
}

private[table] object UpdateBuilder {

  def apply[A, K](implicit table: Table[A, K]): UpdateBuilder[UpdateHasTable, A, K] =
    new UpdateBuilder[UpdateHasTable, A, K](table) {}

  def forTable[A, K](table: Table[A, K]): UpdateBuilder[UpdateHasTable, A, K] =
    new UpdateBuilder[UpdateHasTable, A, K](table) {}
}
