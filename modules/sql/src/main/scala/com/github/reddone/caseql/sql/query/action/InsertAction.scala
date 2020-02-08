package com.github.reddone.caseql.sql.query.action

import cats.implicits._
import com.github.reddone.caseql.sql.query.action.QueryAction.{SQLAction, SQLFragment}
import com.github.reddone.caseql.sql.query.{TableSyntax, TableModifier}
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values}
import doobie._
import Fragment._

class InsertBuilder() {}

object InsertAction {

  sealed abstract class InsertFragment[T, MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val namedFragments = tableModifier
        .entityModifierNamedFragments(modifier)(None)
        .filter(_._2.isEmpty)
        .map {
          case (column, modifier) => (column, modifier.get)
        }
      // TODO: handle empty modifier case
      val valueFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
      val insertFragment = const(s"$InsertInto ${syntax.name} (${namedFragments.map(_._1).mkString(", ")}) $Values")
      insertFragment ++ valueFragment
    }
  }

  final case class One[T, MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, MT](modifier)
      with SQLAction[Int] { self =>

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class OneReturningKey[T, K, MT <: EntityModifier[MT]](
      modifier: MT
  )(
      implicit
      read: Read[K],
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, MT](modifier)
      with SQLAction[K] { self =>

    override def execute: ConnectionIO[K] = {
      self.toFragment.update.withUniqueGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
