package com.github.reddone.caseql.sql.query.action

import cats.implicits._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.query.action.QueryAction.{SQLAction, SQLFragment}
import com.github.reddone.caseql.sql.query.{Table, TableModifier}
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values}
import doobie._
import Fragment._

object InsertAction {

  sealed abstract class InsertFragment[T, K, MT <: EntityModifier[MT]](
      table: Table[T, K],
      modifier: MT
  )(
      implicit tableModifier: TableModifier[T, MT]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val name = table.internalSyntax.name
      val namedFragments = tableModifier
        .entityModifierNamedFragments(modifier)(None)
        .filter(_._2.isEmpty)
        .map {
          case (column, modifier) => (column, modifier.get)
        }
      // TODO: handle empty modifier case
      val insertFragment = const(s"$InsertInto $name (${namedFragments.map(_._1).mkString(", ")}) $Values")
      val valueFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
      insertFragment ++ valueFragment
    }
  }

  final case class One[T, K, MT <: EntityModifier[MT]](table: Table[T, K], modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, K, MT](table, modifier)
      with SQLAction[Int] { self =>

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class OneReturningKey[T, K, MT <: EntityModifier[MT]](
      table: Table[T, K],
      modifier: MT
  )(
      implicit
      read: Read[K],
      tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, K, MT](table, modifier)
      with SQLAction[K] { self =>

    override def execute: ConnectionIO[K] = {
      val syntax = table.internalSyntax // TODO: move this upper
      self.toFragment.update.withUniqueGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
