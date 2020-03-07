package com.github.reddone.caseql.sql.table.query

import cats.implicits._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.{Table, TableModifier}
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values, Default}
import doobie._
import Fragment._

sealed trait InsertBuilderState
sealed trait InsertHasTable    extends InsertBuilderState
sealed trait InsertHasModifier extends InsertBuilderState

sealed abstract class InsertBuilder[S <: InsertBuilderState, A, K](
    table: Table[A, K]
) extends QueryBuilder[A, K](table, None) { self =>

  private[this] var fragment: Fragment = const(
    s"$InsertInto ${querySyntax.name}"
  )

  def withModifier[MA <: EntityModifier[MA]](modifier: MA)(
      implicit
      ev: S =:= InsertHasTable,
      tableModifier: TableModifier[A, MA]
  ): InsertBuilder[S with InsertHasModifier, A, K] = {
    val namedFragments = tableModifier
      .primitiveModifierNamedFragments(modifier)
      .map {
        case (column, Some(modifier)) => (column, modifier)       // one of VALUE, NULL, DEFAULT
        case (column, None)           => (column, const(Default)) // replace None with DEFAULT
      }
    val columnsFragment = const(s"(${namedFragments.map(_._1).mkString(", ")}) $Values")
    val valuesFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
    fragment = fragment ++ columnsFragment ++ valuesFragment
    self.asInstanceOf[InsertBuilder[S with InsertHasModifier, A, K]]
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

object InsertBuilder {

  def apply[A, K](implicit table: Table[A, K]): InsertBuilder[InsertHasTable, A, K] =
    new InsertBuilder[InsertHasTable, A, K](table) {}

  def forTable[A, K](table: Table[A, K]): InsertBuilder[InsertHasTable, A, K] =
    new InsertBuilder[InsertHasTable, A, K](table) {}
}
