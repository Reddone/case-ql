package com.github.reddone.caseql.sql.query.action

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.query.{Syntax, Table, TableFilter, TableQuery}
import com.github.reddone.caseql.sql.query.action.QueryAction._
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

object SelectAction {

  sealed abstract class SelectFragment[T](syntax: Syntax[T])(
      implicit read: Read[T]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")
      selectFragment
    }
  }

  final case class ByFilter[T, FT <: EntityFilter[FT]](syntax: Syntax[T], filter: FT)(
      implicit
      read: Read[T],
      tableFilter: TableFilter[T, FT]
  ) extends SelectFragment[T](syntax)
      with SQLStreamingAction[T] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, T] = {
      self.toFragment.query[T].stream
    }
  }

  final case class ByKey[T, K](syntax: Syntax[T], key: K)(
      implicit
      read: Read[T],
      write: Write[K]
  ) extends SelectFragment[T](syntax)
      with SQLAction[Option[T]] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Option[T]] = {
      self.toFragment.query.option
    }
  }
}
