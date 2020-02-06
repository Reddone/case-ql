package com.github.reddone.caseql.sql.generic

import cats.implicits._
import com.github.reddone.caseql.sql.filter.EntityFilter
import com.github.reddone.caseql.sql.util.FragmentUtils
import com.github.reddone.caseql.sql.tokens.{Update => UpdateToken, _}
import doobie._
import Fragment._
import fs2.Stream

trait TableQuery[T] { table: Table[T] =>

  // WHERE

  def filterFragment[U <: EntityFilter[U]](filter: U, syntax: table.Syntax)(
      implicit tableFilter: TableFilter[T, U]
  ): Option[Fragment] = {
    def make(filter: U): Option[Fragment] = {
      val left  = tableFilter.keys().map(_.name).map(syntax.column)
      val right = tableFilter.values(filter)
      val zipped = left.zip(right).map {
        case (col, optionFilter) => optionFilter.flatMap(_.toOptionFragment(col))
      }
      FragmentUtils.optionalAndOpt(zipped: _*)
    }

    FragmentUtils.optionalAndOpt(
      make(filter),
      filter.AND.flatMap { and =>
        val recs = and.map(filterFragment(_, syntax))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      filter.OR.flatMap { or =>
        val recs = or.map(filterFragment(_, syntax))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      filter.NOT.flatMap { not =>
        val rec = filterFragment(not, syntax)
        FragmentUtils.optionalNot(rec)
      }
    )
  }

  // SELECT

  def selectFragment[U <: EntityFilter[U]](filter: U, syntax: table.Syntax)(
      implicit tableFilter: TableFilter[T, U]
  ): Fragment = {
    val whereFragment  = filterFragment(filter, defaultSyntax).map(const(Where) ++ _).getOrElse(empty)
    val selectFragment = const(s"$Select ${defaultSyntax.columns.mkString(", ")} $From ${defaultSyntax.name}")

    selectFragment ++ whereFragment
  }

  def selectQuery[U <: EntityFilter[U]](filter: U, syntax: table.Syntax)(
      implicit tableFilter: TableFilter[T, U]
  ): ConnectionIO[List[T]] = {
    selectFragment(filter, syntax).query[T].to[List]
  }

  // INSERT

  def insertFragment[U](modifier: U)(
      implicit tableModifier: TableModifier[T, U]
  ): Fragment = {
    val left  = tableModifier.keys().map(_.name).map(table.defaultSyntax.column)
    val right = tableModifier.values(modifier)
    val zipped = left.zip(right).filter(_._2.isDefined).map {
      case (col, optionModifier) => (col, optionModifier.get.toFragment)
    }
    val valueFragment = Fragments.parentheses(zipped.map(_._2).intercalate(const(",")))
    val insertFragment = const(
      s"$InsertInto ${table.defaultSyntax.name} " +
        s"(${zipped.map(_._1).mkString(", ")}) $Values"
    )

    insertFragment ++ valueFragment
  }

  def insertQuery[U](modifier: U)(
      implicit tableModifier: TableModifier[T, U]
  ): ConnectionIO[table.Key] = {
    insertFragment(modifier).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // UPDATE

  def updateFragment[U, V <: EntityFilter[V]](modifier: U, filter: V)(
      implicit
      tableModifier: TableModifier[T, U],
      tableFilter: TableFilter[T, V]
  ): Fragment = {
    val left  = tableModifier.keys().map(_.name).map(table.defaultSyntax.column)
    val right = tableModifier.values(modifier)
    val zipped = left.zip(right).filter(_._2.isDefined).map {
      case (col, optionModifier) => (col, optionModifier.get.toFragment)
    }
    val joined         = zipped.map { case (col, parameter) => const(col + " =") ++ parameter }
    val whereFragment  = filterFragment(filter, table.defaultSyntax).map(const(Where) ++ _).getOrElse(empty)
    val setFragment    = Fragments.set(joined: _*)
    val updateFragment = const(s"$UpdateToken ${table.defaultSyntax.name}")

    updateFragment ++ setFragment ++ whereFragment
  }

  def updateQuery[U, V <: EntityFilter[V]](modifier: U, filter: V)(
      implicit
      tableModifier: TableModifier[T, U],
      tableFilter: TableFilter[T, V]
  ): ConnectionIO[table.Key] = {
    updateFragment(modifier, filter).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // DELETE

  def deleteFragment[U <: EntityFilter[U]](filter: U)(
      implicit tableFilter: TableFilter[T, U]
  ): Fragment = {
    val whereFragment  = filterFragment(filter, table.defaultSyntax).map(const(Where) ++ _).getOrElse(empty)
    val deleteFragment = const(s"$Delete $From ${table.defaultSyntax.name}")

    deleteFragment ++ whereFragment
  }

  def deleteQuery[U <: EntityFilter[U]](filter: U)(
      implicit tableFilter: TableFilter[T, U]
  ): ConnectionIO[table.Key] = {
    deleteFragment(filter).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }
}
