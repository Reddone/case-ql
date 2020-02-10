package com.github.reddone.caseql.sql.table.query

import cats.implicits._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query.Query.{QueryBuilder, SQLAction}
import com.github.reddone.caseql.sql.table.{Table, TableModifier}
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values}
import doobie._
import Fragment._

sealed trait InsertBuilderState
sealed trait InsertHasTable    extends InsertBuilderState
sealed trait InsertHasModifier extends InsertBuilderState

private[table] class InsertBuilder[S <: InsertBuilderState, T, K](
    table: Table[T, K]
) extends QueryBuilder[T, K](table, None) { self =>

  private[this] var fragment: Fragment = const(
    s"$InsertInto ${querySyntax.name}"
  )

  def withModifier[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      ev: S =:= InsertHasTable,
      tableModifier: TableModifier[T, MT]
  ): InsertBuilder[S with InsertHasModifier, T, K] = {
    val namedFragments = tableModifier
      .entityModifierNamedFragments(modifier)(None)
      .filter(_._2.isEmpty)
      .map {
        case (column, modifier) => (column, modifier.get)
      }
    // TODO: handle empty modifier case, because all Option[Modifier[_] are empty
    val columnsFragment = const(s"(${namedFragments.map(_._1).mkString(", ")}) $Values")
    val valuesFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
    fragment = fragment ++ columnsFragment ++ valuesFragment
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
      override def toFragment: Fragment = fragment
      override def execute: ConnectionIO[K] =
        fragment.update.withUniqueGeneratedKeys[K](querySyntax.keyColumns: _*)(table.keyRead)
    }
}

private[table] object InsertBuilder {

  def forTable[T, K](table: Table[T, K]) = new InsertBuilder[InsertHasTable, T, K](table)
}
