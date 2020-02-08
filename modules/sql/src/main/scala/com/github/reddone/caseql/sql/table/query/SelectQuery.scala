package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.query.Action._
import com.github.reddone.caseql.sql.table.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

object SelectQuery {

  sealed abstract class SelectFragment[T, K](
      table: Table[T, K],
      alias: Option[String]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val syntax         = table.internalSyntax // TODO: use alias to change syntax
      val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")
      selectFragment
    }
  }

  final case class ByFilter[T, K, FT <: EntityFilter[FT]](
      table: Table[T, K],
      filter: FT,
      alias: Option[String]
  )(
      implicit
      read: Read[T],
      tableFilter: TableFilter[T, FT]
  ) extends SelectFragment[T, K](table, alias)
      with SQLStreamingAction[T] { self =>

    override def toFragment: Fragment = {
      val whereFragment = table
        .byFilterFragment(filter, alias)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, T] = {
      self.toFragment.query[T].stream
    }
  }

  final case class ByKey[T, K](
      table: Table[T, K],
      key: K,
      alias: Option[String]
  )(
      implicit
      read: Read[T],
      write: Write[K]
  ) extends SelectFragment[T, K](table, alias)
      with SQLAction[Option[T]] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Option[T]] = {
      self.toFragment.query.option
    }
  }
}
