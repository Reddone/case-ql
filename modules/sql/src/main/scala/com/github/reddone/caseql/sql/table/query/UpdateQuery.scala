package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query.Query._
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableModifier, TableSyntax}
import com.github.reddone.caseql.sql.tokens.Where
import doobie._
import Fragment._
import fs2.Stream

sealed trait UpdateBuilderState
sealed trait UpdateHasTable    extends UpdateBuilderState
sealed trait UpdateHasModifier extends UpdateBuilderState
sealed trait UpdateHasFilter   extends UpdateBuilderState
sealed trait UpdateHasKey      extends UpdateBuilderState

private[table] class UpdateBuilder[S <: UpdateBuilderState, T, K](
    table: Table[T, K],
    alias: Option[String]
) { self =>

  private[this] var fragment: Fragment = {
    // TODO: use alias
    const(s"$Update ${table.internalSyntax.name}")
  }

  private val syntax: TableSyntax[T] = table.internalSyntax

  def withModifier[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      ev: S =:= UpdateHasTable,
      tableModifier: TableModifier[T, MT]
  ): UpdateBuilder[S with UpdateHasModifier, T, K] = {
    val namedFragments = tableModifier
      .entityModifierNamedFragments(modifier)(alias)
      .filter(_._2.isEmpty)
      .map {
        case (column, modifier) => (column, modifier.get)
      }
    // TODO: handle empty modifier case
    val setFragment = Fragments.set(namedFragments.map {
      case (col, parameter) => const(col + " =") ++ parameter
    }: _*) // love scala emojis
    fragment = fragment ++ setFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasModifier, T, K]]
  }

  def withFilter[FT <: EntityFilter[FT]](filter: FT)(
      implicit
      ev: S =:= UpdateHasTable with UpdateHasModifier,
      tableFilter: TableFilter[T, FT]
  ): UpdateBuilder[S with UpdateHasFilter, T, K] = {
    val whereFragment = table
      .byFilterFragment(filter, alias)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    fragment = fragment ++ whereFragment
    self.asInstanceOf[UpdateBuilder[S with UpdateHasFilter, T, K]]
  }

  def withKey(key: K)(
      implicit ev: S =:= UpdateHasTable with UpdateHasModifier
  ): UpdateBuilder[S with UpdateHasKey, T, K] = {
    val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
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
    override def toFragment: Fragment             = fragment
    override def execute: Stream[ConnectionIO, K] = fragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
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
    override def toFragment: Fragment             = fragment
    override def execute: Stream[ConnectionIO, K] = fragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
  }
}

private[table] object UpdateBuilder {

  def forTable[T, K](table: Table[T, K], alias: Option[String]) = new UpdateBuilder[UpdateHasTable, T, K](table, alias)
}
