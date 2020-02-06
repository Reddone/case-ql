package com.github.reddone.caseql.sql.generic

import cats.implicits._
import com.github.reddone.caseql.sql.util.FragmentUtils
import com.github.reddone.caseql.sql.tokens.{Update => UpdateToken, _}
import doobie._
import Fragment._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import fs2.Stream

trait TableQuery[T] { table: Table[T] =>

  // WHERE

  def filterFragment[FT <: EntityFilter[T, FT]](filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Option[Fragment] = {
    FragmentUtils.optionalAndOpt(
      FragmentUtils.optionalAndOpt(tableFilter.entityFilterFragments(filter): _*),
      FragmentUtils.optionalAndOpt(tableFilter.relationFilterFragments(filter): _*),
      filter.AND.flatMap { and =>
        val recs = and.map(filterFragment(_))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      filter.OR.flatMap { or =>
        val recs = or.map(filterFragment(_))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      filter.NOT.flatMap { not =>
        val rec = filterFragment(not)
        FragmentUtils.optionalNot(rec)
      }
    )
  }

  // SELECT

  def selectFragment[FT <: EntityFilter[T, FT]](filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val whereFragment  = filterFragment(filter).map(const(Where) ++ _).getOrElse(empty)
    val selectFragment = const(s"$Select ${defaultSyntax.columns.mkString(", ")} $From ${defaultSyntax.name}")

    selectFragment ++ whereFragment
  }

  def selectQuery[FT <: EntityFilter[T, FT]](filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): ConnectionIO[List[T]] = {
    selectFragment(filter).query[T].to[List]
  }

  // INSERT

  def insertFragment[MT <: EntityModifier[T, MT]](modifier: MT)(
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

  def insertQuery[MT <: EntityModifier[T, MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): ConnectionIO[table.Key] = {
    insertFragment(modifier).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // UPDATE

  def updateFragment[MT <: EntityModifier[T, MT], FT <: EntityFilter[T, FT]](modifier: MT, filter: FT)(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val namedFragments = tableModifier.entityModifierNamedFragments(modifier).filter(_._2.isEmpty).map {
      case (column, modifier) => (column, modifier.get)
    }
    // TODO: handle empty modifier
    val joined         = namedFragments.map { case (col, parameter) => const(col + " =") ++ parameter }
    val whereFragment  = filterFragment(filter).map(const(Where) ++ _).getOrElse(empty)
    val setFragment    = Fragments.set(joined: _*)
    val updateFragment = const(s"$UpdateToken ${table.defaultSyntax.name}")

    updateFragment ++ setFragment ++ whereFragment
  }

  def updateQuery[MT <: EntityModifier[T, MT], FT <: EntityFilter[T, FT]](modifier: MT, filter: FT)(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): ConnectionIO[table.Key] = {
    updateFragment(modifier, filter).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }

  // DELETE

  def deleteFragment[FT <: EntityFilter[T, FT]](filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Fragment = {
    val whereFragment  = filterFragment(filter).map(const(Where) ++ _).getOrElse(empty)
    val deleteFragment = const(s"$Delete $From ${table.defaultSyntax.name}")

    deleteFragment ++ whereFragment
  }

  def deleteQuery[FT <: EntityFilter[T, FT]](filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): ConnectionIO[table.Key] = {
    deleteFragment(filter).update.withUniqueGeneratedKeys[table.Key](table.defaultSyntax.keyColumns: _*)
  }
}
