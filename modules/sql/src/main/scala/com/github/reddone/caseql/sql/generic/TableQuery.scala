package com.github.reddone.caseql.sql.generic

import cats.implicits._
import com.github.reddone.caseql.sql.util.FragmentUtils
import com.github.reddone.caseql.sql.tokens.{And, Placeholder, Update => UpdateToken, _}
import doobie._
import Fragment._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.generic.ops.QueryOps
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import fs2.Stream

trait TableQuery[T] { table: Table[T] =>

  // SELECT

  def selectFragment[FT <: EntityFilter[FT]](syntax: table.Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val whereFragment  = QueryOps.byFilterConditionFragment(syntax, filter).map(const(Where) ++ _).getOrElse(empty)
    val selectFragment = const(s"$Select ${syntax.columns.mkString(", ")} $From ${syntax.name}")

    selectFragment ++ whereFragment
  }

  def selectQuery[FT <: EntityFilter[FT]](syntax: table.Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): ConnectionIO[List[T]] = {
    selectFragment(syntax, filter).query[T].to[List]
  }

  // INSERT

  def insertFragment[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): Fragment = {
    val namedFragments = tableModifier.entityModifierNamedFragments(modifier).filter(_._2.isEmpty).map {
      case (column, modifier) => (column, modifier.get)
    }
    // TODO: handle empty modifier
    val valueFragment = Fragments.parentheses(namedFragments.map(_._2).intercalate(const(",")))
    val insertFragment = const(
      s"$InsertInto ${table.defaultSyntax.name} " +
        s"(${namedFragments.map(_._1).mkString(", ")}) $Values"
    )

    insertFragment ++ valueFragment
  }

  def insertQuery[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): ConnectionIO[table.Key] = {
    insertFragment(modifier).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // UPDATE

  def updateFragment[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](modifier: MT, filter: FT)(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val namedFragments = tableModifier.entityModifierNamedFragments(modifier).filter(_._2.isEmpty).map {
      case (column, modifier) => (column, modifier.get)
    }
    // TODO: handle empty modifier
    val joined = namedFragments.map { case (col, parameter) => const(col + " =") ++ parameter }
    val whereFragment =
      QueryOps.byFilterConditionFragment(table.defaultSyntax, filter).map(const(Where) ++ _).getOrElse(empty)
    val setFragment    = Fragments.set(joined: _*)
    val updateFragment = const(s"$UpdateToken ${table.defaultSyntax.name}")

    updateFragment ++ setFragment ++ whereFragment
  }

  def updateQuery[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](modifier: MT, filter: FT)(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): ConnectionIO[table.Key] = {
    updateFragment(modifier, filter).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // DELETE

  def deleteFragment[FT <: EntityFilter[FT]](syntax: table.Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val whereFragment  = QueryOps.byFilterConditionFragment(syntax, filter).map(const(Where) ++ _).getOrElse(empty)
    val deleteFragment = const(s"$Delete $From ${syntax.name}")

    deleteFragment ++ whereFragment
  }

  def deleteQuery[FT <: EntityFilter[FT]](syntax: table.Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): ConnectionIO[table.Key] = {
    deleteFragment(syntax, filter).update.withUniqueGeneratedKeys[table.Key](syntax.keyColumns: _*)
  }
}
