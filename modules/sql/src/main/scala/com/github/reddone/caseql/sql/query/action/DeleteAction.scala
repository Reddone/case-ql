package com.github.reddone.caseql.sql.query.action

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.query.action.QueryAction._
import com.github.reddone.caseql.sql.query.{Syntax, Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import fs2.Stream

object DeleteAction {

  sealed abstract class DeleteFragment[T](syntax: Syntax[T]) extends SQLFragment {

    override def toFragment: Fragment = {
      val deleteFragment = const(s"$Delete $From ${syntax.name}")
      deleteFragment
    }
  }

  final case class ByFilter[T, FT <: EntityFilter[FT]](syntax: Syntax[T], filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T](syntax)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByFilterReturningKeys[T, K, FT <: EntityFilter[FT]](
      syntax: Syntax[T],
      filter: FT,
      token: Class[K]
  )(
      implicit
      read: Read[K],
      tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T](syntax)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }

  final case class ByKey[T, K](syntax: Syntax[T], key: K)(
      implicit read: Write[K]
  ) extends DeleteFragment[T](syntax)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByKeyReturningKeys[T, K](syntax: Syntax[T], key: K)(
      implicit
      read: Read[K],
      write: Write[K]
  ) extends DeleteFragment[T](syntax)
      with SQLStreamingAction[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
