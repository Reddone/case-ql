package com.github.reddone.caseql.sql.query.action

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.query.action.QueryAction._
import com.github.reddone.caseql.sql.query.{TableSyntax, TableFilter, TableModifier}
import com.github.reddone.caseql.sql.tokens.Where
import doobie._
import Fragment._
import fs2.Stream

object UpdateAction {

  sealed abstract class UpdateFragment[T, MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val namedFragments = tableModifier
        .entityModifierNamedFragments(modifier)(Some(syntax.alias))
        .filter(_._2.isEmpty)
        .map {
          case (column, modifier) => (column, modifier.get)
        }
      // TODO: handle empty modifier case
      val joined         = namedFragments.map { case (col, parameter) => const(col + " =") ++ parameter }
      val setFragment    = Fragments.set(joined: _*)
      val updateFragment = const(s"$Update ${syntax.name}")

      updateFragment ++ setFragment
    }
  }

  final case class ByFilter[T, MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT
  )(
      implicit
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ) extends UpdateFragment[T, MT](modifier)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByFilterReturningKeys[T, K, MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT
  )(
      implicit
      read: Read[K],
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ) extends UpdateFragment[T, MT](modifier)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }

  final case class ByKey[T, K, MT <: EntityModifier[MT]](
      modifier: MT,
      key: K
  )(
      implicit
      write: Write[K],
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends UpdateFragment[T, MT](modifier)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByKeyReturningKeys[T, K, MT <: EntityModifier[MT]](
      modifier: MT,
      key: K
  )(
      implicit
      read: Read[K],
      write: Write[K],
      syntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ) extends UpdateFragment[T, MT](modifier)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
