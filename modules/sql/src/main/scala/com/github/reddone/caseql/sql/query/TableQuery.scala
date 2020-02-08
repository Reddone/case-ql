package com.github.reddone.caseql.sql.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.query.action.{DeleteAction, InsertAction, SelectAction, UpdateAction}
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie.Fragment

trait TableQuery[T, K] { table: Table[T, K] =>

  // SELECT

  final def select[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): SelectAction.ByFilter[T, K, FT] = {
    SelectAction.ByFilter(table, filter, alias)
  }

  final def selectByKey(key: K, alias: Option[String] = None): SelectAction.ByKey[T, K] = {
    SelectAction.ByKey(table, key, alias)
  }

  // INSERT

  final def insertOne[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): InsertAction.One[T, K, MT] = {
    InsertAction.One(table, modifier)
  }

  final def insertOneReturningKey[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): InsertAction.OneReturningKey[T, K, MT] = {
    InsertAction.OneReturningKey(table, modifier)
  }

  // UPDATE

  final def update[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT,
      alias: Option[String] = None
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilter[T, K, MT, FT] = {
    UpdateAction.ByFilter(table, modifier, filter, alias)
  }

  final def updateReturningKeys[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT,
      alias: Option[String] = None
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilterReturningKeys[T, K, MT, FT] = {
    UpdateAction.ByFilterReturningKeys(table, modifier, filter, alias)
  }

  final def updateByKey[MT <: EntityModifier[MT]](modifier: MT, key: K, alias: Option[String] = None)(
      implicit tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKey[T, K, MT] = {
    UpdateAction.ByKey(table, modifier, key, alias)
  }

  final def updateByKeyReturningKeys[MT <: EntityModifier[MT]](modifier: MT, key: K, alias: Option[String] = None)(
      implicit tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKeyReturningKeys[T, K, MT] = {
    UpdateAction.ByKeyReturningKeys(table, modifier, key, alias)
  }

  // DELETE

  final def delete[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteAction.ByFilter[T, K, FT] = {
    DeleteAction.ByFilter(table, filter, alias)
  }

  final def deleteReturningKeys[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteAction.ByFilterReturningKeys[T, K, FT] = {
    DeleteAction.ByFilterReturningKeys(table, filter, alias)
  }

  final def deleteByKey(key: K, alias: Option[String] = None): DeleteAction.ByKey[T, K] = {
    DeleteAction.ByKey(table, key, alias)
  }

  final def deleteByKeyReturningKeys(key: K, alias: Option[String] = None): DeleteAction.ByKeyReturningKeys[T, K] = {
    DeleteAction.ByKeyReturningKeys(table, key, alias)
  }

  // WHERE

  final def byFilterFragment[FT <: EntityFilter[FT]](filter: FT, alias: Option[String])(
      implicit tableFilter: TableFilter[T, FT]
  ): Option[Fragment] = {
    // AND between all possible filters
    FragmentUtils.optionalAndOpt(
      // AND between all Option[Filter[_]]
      FragmentUtils.optionalAndOpt(tableFilter.entityFilterFragments(filter)(alias): _*),
      // AND between all Option[RelationFilter[T, _, _]]
      FragmentUtils.optionalAndOpt(tableFilter.relationFilterFragments(filter)(alias): _*),
      // AND between all Option[EntityFilter[T]] using self recursive type
      filter.AND.flatMap { and =>
        val recs = and.map(byFilterFragment(_, alias))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      // OR between all Option[EntityFilter[T]] using self recursive type
      filter.OR.flatMap { or =>
        val recs = or.map(byFilterFragment(_, alias))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      // NOT for one Option[EntityFilter[T]] using self recursive type
      filter.NOT.flatMap { not =>
        val rec = byFilterFragment(not, alias)
        FragmentUtils.optionalNot(rec)
      }
    )
  }

  final def byKeyFragment(key: K, alias: Option[String]): Fragment = {
    keyWrite.toFragment(key, internalSyntax.keyColumns.map(col => s"$col = $Placeholder").mkString(s" $And "))
  }
}
