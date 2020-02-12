package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableModifier}
import com.github.reddone.caseql.sql.tokens.{From, Update => UpdateToken, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait UpdateBuilderState
sealed trait UpdateHasTable    extends UpdateBuilderState
sealed trait UpdateHasModifier extends UpdateBuilderState
sealed trait UpdateHasFilter   extends UpdateBuilderState
sealed trait UpdateHasKey      extends UpdateBuilderState

final class UpdateBuilder[S <: UpdateBuilderState, T, K](
    table: Table[T, K]
) extends QueryBuilder[T, K](table, None) { self =>

  private[this] var fragment: Fragment = const(
    s"$UpdateToken ${querySyntax.name}"
  )

  def withModifier[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      ev: S =:= UpdateHasTable,
      tableModifier: TableModifier[T, MT]
  ): UpdateBuilder[S with UpdateHasModifier, T, K] = {
    val namedFragments = tableModifier
      .entityModifierNamedFragments(modifier)(querySyntax.alias)
      .filter(_._2.nonEmpty)
      .map {
        case (column, modifier) => (column, modifier.get)
      }
    val setFragment = Fragments.set(namedFragments.map {
      case (col, param) => const(col + " =") ++ param
    }: _*) // love scala emojis
    fragment = fragment ++ setFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasModifier, T, K]]
  }

  def withFilter[FT <: EntityFilter[FT]](filter: FT)(
      implicit
      ev: S =:= UpdateHasTable with UpdateHasModifier,
      tableFilter: TableFilter[T, FT]
  ): UpdateBuilder[S with UpdateHasFilter, T, K] = {
    val whereFragment = tableFilter
      .byFilterFragment(filter, querySyntax.alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasFilter, T, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier
  ): UpdateBuilder[S with UpdateHasKey, T, K] = {
    val whereFragment = const(Where) ++ byKeyFragment(key)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasKey, T, K]]
  }

  def buildUpdate(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasFilter
  ): SQLAction[Int] = new SQLAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildUpdateReturningKeys(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasFilter
  ): SQLStreamingAction[K] = new SQLStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }

  def buildUpdateByKey(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasKey
  ): SQLAction[Int] = new SQLAction[Int] {
    override def toFragment: Fragment       = fragment
    override def execute: ConnectionIO[Int] = fragment.update.run
  }

  def buildUpdateByKeyReturningKeys(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier with UpdateHasKey
  ): SQLStreamingAction[K] = new SQLStreamingAction[K] {
    override def toFragment: Fragment = fragment
    override def execute: Stream[ConnectionIO, K] =
      fragment.update.withGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
  }
}

private[table] object UpdateBuilder {

  def apply[T, K](implicit table: Table[T, K]) = new UpdateBuilder[UpdateHasTable, T, K](table)

  def forTable[T, K](table: Table[T, K]) = new UpdateBuilder[UpdateHasTable, T, K](table)
}
