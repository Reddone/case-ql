package com.github.reddone.caseql.sql.generic.ops

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.generic.{Table, TableFilter, TableQuery}
import com.github.reddone.caseql.sql.generic.ops.QueryOps._
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

object SelectOps {

  sealed abstract class SelectFragment[T](syntax: Table[T]#Syntax)(
      implicit read: Read[T]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")
      selectFragment
    }
  }

  final case class ByFilter[T, FT <: EntityFilter[FT]](syntax: Table[T]#Syntax, filter: FT)(
      implicit
      read: Read[T],
      tableFilter: TableFilter[T, FT]
  ) extends SelectFragment[T](syntax)
      with SQLStreamingQuery[T] { self =>

    override def toFragment: Fragment = {
      val whereFragment = FilterOps
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, T] = {
      self.toFragment.query[T].stream
    }
  }

  final case class ByKey[T, K <: Table[T]#Key](syntax: Table[T]#Syntax, key: K)(
      implicit
      read: Read[T],
      write: Write[K]
  ) extends SelectFragment[T](syntax)
      with SQLQuery[Option[T]] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ FilterOps.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Option[T]] = {
      self.toFragment.query.option
    }
  }
}
