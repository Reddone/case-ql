package com.github.reddone.caseql.sql.generic.ops

import cats.implicits._
import com.github.reddone.caseql.sql.generic.ops.QueryOps.{SQLFragment, SQLQuery}
import com.github.reddone.caseql.sql.generic.{Table, TableModifier}
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.tokens.{InsertInto, Values}
import doobie._
import Fragment._

object InsertOps {

  sealed abstract class InsertFragment[T, MT <: EntityModifier[MT]](syntax: Table[T]#Syntax, modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val namedFragments = tableModifier.entityModifierNamedFragments(modifier).filter(_._2.isEmpty).map {
        case (column, modifier) => (column, modifier.get)
      }
      // TODO: handle empty modifier case
      val valueFragment  = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
      val insertFragment = const(s"$InsertInto ${syntax.name} (${namedFragments.map(_._1).mkString(", ")}) $Values")
      insertFragment ++ valueFragment
    }
  }

  final case class One[T, MT <: EntityModifier[MT]](syntax: Table[T]#Syntax, modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, MT](syntax, modifier)
      with SQLQuery[Int] { self =>

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class OneReturningKey[T, MT <: EntityModifier[MT], K <: Table[T]#Key](
      syntax: Table[T]#Syntax,
      modifier: MT
  )(
      implicit
      read: Read[K],
      tableModifier: TableModifier[T, MT]
  ) extends InsertFragment[T, MT](syntax, modifier)
      with SQLQuery[K] { self =>

    override def execute: ConnectionIO[K] = {
      self.toFragment.update.withUniqueGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
