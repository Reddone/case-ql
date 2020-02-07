package com.github.reddone.caseql.sql.generic.ops

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.generic.{Table, TableFilter, TableQuery}
import com.github.reddone.caseql.sql.tokens.{Delete, From, Where}
import doobie._
import Fragment._
import com.github.reddone.caseql.sql.generic.ops.QueryOps._

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
      val whereFragment = FilterOps
        .byFilterConditionFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Int] = {
      self.toFragment.update.run
    }
  }

  final case class ByFilterReturningKeys()

  final case class ByKey()

  final case class ByKeyReturningKeys()
}
