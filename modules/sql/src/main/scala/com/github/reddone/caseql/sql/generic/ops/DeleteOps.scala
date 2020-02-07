package com.github.reddone.caseql.sql.generic.ops

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.generic.ops.QueryOps._
import com.github.reddone.caseql.sql.generic.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import fs2.Stream

object DeleteOps {

  sealed abstract class DeleteFragment[T](syntax: Table[T]#Syntax) extends SQLFragment {

    override def toFragment: Fragment = {
      val deleteFragment = const(s"$Delete $From ${syntax.name}")
      deleteFragment
    }
  }

  final case class ByFilter[T, FT <: EntityFilter[FT]](syntax: Table[T]#Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T](syntax)
      with SQLQuery[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryOps
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByFilterReturningKeys[T, FT <: EntityFilter[FT], K <: Table[T]#Key](
      syntax: Table[T]#Syntax,
      filter: FT
  )(
      implicit
      read: Read[K],
      tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T](syntax)
      with SQLStreamingQuery[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryOps
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }

  final case class ByKey[T, K <: Table[T]#Key](syntax: Table[T]#Syntax, key: K)(
      implicit read: Write[K]
  ) extends DeleteFragment[T](syntax)
      with SQLQuery[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryOps.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByKeyReturningKeys[T, K <: Table[T]#Key](syntax: Table[T]#Syntax, key: K)(
      implicit
      read: Read[K],
      write: Write[K]
  ) extends DeleteFragment[T](syntax)
      with SQLStreamingQuery[K] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryOps.byKeyConditionFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, K] = {
      self.toFragment.update.withGeneratedKeys[K](syntax.keyColumns: _*)
    }
  }
}
