package com.github.reddone.caseql.sql.query.action

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.query.action.QueryAction._
import com.github.reddone.caseql.sql.query.{Table, TableFilter, TableModifier, TableSyntax}
import com.github.reddone.caseql.sql.tokens.Where
import doobie._
import Fragment._
import fs2.Stream

object UpdateAction {

  sealed abstract class UpdateFragment[T, K, MT <: EntityModifier[MT]](
      table: Table[T, K],
      modifier: MT,
      alias: Option[String]
  )(
      implicit tableModifier: TableModifier[T, MT]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val name = table.internalSyntax.name
      val namedFragments = tableModifier
        .entityModifierNamedFragments(modifier)(alias)
        .filter(_._2.isEmpty)
        .map {
          case (column, modifier) => (column, modifier.get)
        }
      // TODO: handle empty modifier case
      val updateFragment = const(s"$Update $name")
      val setFragment = Fragments.set(namedFragments.map {
        case (col, parameter) => const(col + " =") ++ parameter
      }: _*) // love scala emojis

      updateFragment ++ setFragment
    }
  }

  final case class ByFilter[T, K, MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      table: Table[T, K],
      modifier: MT,
      filter: FT,
      alias: Option[String]
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ) extends UpdateFragment[T, K, MT](table, modifier, alias)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = table
        .byFilterFragment(filter, alias)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByFilterReturningKeys[T, K, MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      table: Table[T, K],
      modifier: MT,
      filter: FT,
      alias: Option[String]
  )(
      implicit
      read: Read[K],
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ) extends UpdateFragment[T, K, MT](table, modifier, alias)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = table
        .byFilterFragment(filter, alias)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      val syntax = table.internalSyntax // TODO: add alias
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }

  final case class ByKey[T, K, MT <: EntityModifier[MT]](
      table: Table[T, K],
      modifier: MT,
      key: K,
      alias: Option[String]
  )(
      implicit
      write: Write[K],
      tableModifier: TableModifier[T, MT]
  ) extends UpdateFragment[T, K, MT](table, modifier, alias)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByKeyReturningKeys[T, K, MT <: EntityModifier[MT]](
      table: Table[T, K],
      modifier: MT,
      key: K,
      alias: Option[String]
  )(
      implicit
      read: Read[K],
      write: Write[K],
      tableModifier: TableModifier[T, MT]
  ) extends UpdateFragment[T, K, MT](table, modifier, alias)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      val syntax = table.internalSyntax // TODO: add alias
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
