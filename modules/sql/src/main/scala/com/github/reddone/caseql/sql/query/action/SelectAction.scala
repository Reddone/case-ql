package com.github.reddone.caseql.sql.query.action

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.query.action.QueryAction._
import com.github.reddone.caseql.sql.query.{Table, TableFilter, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{From, Select, Where}
import doobie._
import Fragment._
import fs2.Stream

sealed trait SelectBuilderState

class SelectBuilder[S <: SelectBuilderState](
    private var fragment: Fragment,
) {

  def withSyntax[T, K](table: Table[T, K])(
  ): SelectBuilder[T] = {
    val selectFragment = const(s"$Select ${tableSyntax.columns.mkString(", ")} $From ${tableSyntax.name}")
    new SelectBuilder(fragment ++ selectFragment, Some(tableSyntax), alias)
  }

  def withFilter[T, FT <: EntityFilter[FT]](filter: FT, tableFilter: TableFilter[T, FT]): SelectBuilder[T] = {
    val whereFragment = QueryAction
      .byFilterFragment(syntax, filter)
      .map(const(Where) ++ _)
      .getOrElse(empty)
    new SelectBuilder(fragment ++ whereFragment)
  }

  def withFilter[K](key: K)(
      implicit write: Write[K]
  ): SelectBuilder = {
    this
  }

}

object SelectAction {

  sealed abstract class SelectFragment[T](syntax: TableSyntax[T])(
      implicit read: Read[T]
  ) extends SQLFragment {

    override def toFragment: Fragment = {
      val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")
      selectFragment
    }
  }

  final case class ByFilter[T, FT <: EntityFilter[FT]](syntax: TableSyntax[T], filter: FT)(
      implicit
      read: Read[T],
      tableFilter: TableFilter[T, FT]
  ) extends SelectFragment[T](syntax)
      with SQLStreamingAction[T] { self =>

    override def toFragment: Fragment = {
      val whereFragment = QueryAction
        .byFilterFragment(syntax, filter)
        .map(const(Where) ++ _)
        .getOrElse(empty)
      super.toFragment ++ whereFragment
    }

    override def execute: Stream[ConnectionIO, T] = {
      self.toFragment.query[T].stream
    }
  }

  final case class ByKey[T, K](syntax: TableSyntax[T], key: K)(
      implicit
      read: Read[T],
      write: Write[K]
  ) extends SelectFragment[T](syntax)
      with SQLAction[Option[T]] { self =>

    override def toFragment: Fragment = {
      val whereFragment = const(Where) ++ QueryAction.byKeyFragment(syntax, key)
      super.toFragment ++ whereFragment
    }

    override def execute: ConnectionIO[Option[T]] = {
      self.toFragment.query.option
    }
  }
}
