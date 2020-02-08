package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.query.Action._
import com.github.reddone.caseql.sql.table.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import fs2.Stream

object DeleteQuery {

  sealed abstract class DeleteFragment[T, K](
      table: Table[T, K],
      alias: Option[String]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val name           = table.internalSyntax.name
      val deleteFragment = const(s"$Delete $From $name")
      deleteFragment
    }
  }

  final case class ByFilter[T, K, FT <: EntityFilter[FT]](
      table: Table[T, K],
      filter: FT,
      alias: Option[String]
  )(
      implicit tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T, K](table, alias)
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

  final case class ByFilterReturningKeys[T, K, FT <: EntityFilter[FT]](
      table: Table[T, K],
      filter: FT,
      alias: Option[String]
  )(
      implicit
      read: Read[K],
      tableFilter: TableFilter[T, FT]
  ) extends DeleteFragment[T, K](table, alias)
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

  final case class ByKey[T, K](
      table: Table[T, K],
      key: K,
      alias: Option[String]
  )(
      implicit read: Write[K]
  ) extends DeleteFragment[T, K](table, alias)
      with SQLAction[Int] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ table.byKeyFragment(key, alias)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByKeyReturningKeys[T, K](
      table: Table[T, K],
      key: K,
      alias: Option[String]
  )(
      implicit
      read: Read[K],
      write: Write[K]
  ) extends DeleteFragment[T, K](table, alias)
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
