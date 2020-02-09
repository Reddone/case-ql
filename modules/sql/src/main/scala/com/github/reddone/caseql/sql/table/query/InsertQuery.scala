package com.github.reddone.caseql.sql.table.query

import cats.implicits._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query.Query.SQLAction
import com.github.reddone.caseql.sql.table.{Table, TableModifier, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values}
import doobie._
import Fragment._

sealed trait InsertBuilderState
sealed trait InsertHasTable    extends InsertBuilderState
sealed trait InsertHasModifier extends InsertBuilderState

private[table] class InsertBuilder[S <: InsertBuilderState, T, K](
    table: Table[T, K]
) { self =>

  private[this] var fragment: Fragment = {
    const(s"$InsertInto ${table.internalSyntax.name}")
  }

  private val syntax: TableSyntax[T] = table.internalSyntax

  def withModifier[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      ev: S =:= InsertHasTable,
      tableModifier: TableModifier[T, MT]
  ): InsertBuilder[S with InsertHasModifier, T, K] = {
    val name = table.internalSyntax.name
    val namedFragments = tableModifier
      .entityModifierNamedFragments(modifier)(None)
      .filter(_._2.isEmpty)
      .map {
        case (column, modifier) => (column, modifier.get)
      }
    // TODO: handle empty modifier case
    val insertFragment = const(s"(${namedFragments.map(_._1).mkString(", ")}) $Values")
    val valueFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
    fragment = fragment ++ insertFragment ++ valueFragment
    self.asInstanceOf[InsertBuilder[S with InsertHasModifier, T, K]]
  }

  def buildInsertOne(
      implicit ev: S =:= InsertHasTable with InsertHasModifier
  ): SQLAction[Int] =
    new SQLAction[Int] {
      override def toFragment: Fragment       = fragment
      override def execute: ConnectionIO[Int] = fragment.update.run
    }

  def buildInsertOneReturningKey(
      implicit ev: S =:= InsertHasTable with InsertHasModifier
  ): SQLAction[K] =
    new SQLAction[K] {
      override def toFragment: Fragment     = fragment
      override def execute: ConnectionIO[K] = fragment.update.withUniqueGeneratedKeys[K](syntax.keyColumns: _*)
    }
}

private[table] object InsertBuilder {

  def forTable[T, K](table: Table[T, K]) = new InsertBuilder[InsertHasTable, T, K](table)
}
