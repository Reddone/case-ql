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
      implicit
      tableSyntax: TableSyntax[T],
      tableFilter: TableFilter[T, FT]
  ): SelectAction.ByFilter[T, FT] = {
    SelectAction.ByFilter[T, FT](tableSyntax, filter)
  }

  final def selectByKey(key: K, alias: Option[String] = None)(
      implicit tableSyntax: TableSyntax[T]
  ): SelectAction.ByKey[T, K] = {
    SelectAction.ByKey[T, K](tableSyntax, key)
  }

  // INSERT

  final def insertOne[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ): InsertAction.One[T, MT] = {
    InsertAction.One[T, MT](modifier)
  }

  final def insertOneReturningKey[MT <: EntityModifier[MT]](modifier: MT)(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ): InsertAction.OneReturningKey[T, K, MT] = {
    InsertAction.OneReturningKey[T, K, MT](modifier)
  }

  // UPDATE

  final def update[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT,
      alias: Option[String] = None
  )(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilter[T, MT, FT] = {
    UpdateAction.ByFilter[T, MT, FT](modifier, filter)
  }

  final def updateReturningKeys[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT,
      alias: Option[String] = None
  )(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilterReturningKeys[T, K, MT, FT] = {
    UpdateAction.ByFilterReturningKeys[T, K, MT, FT](modifier, filter)
  }

  final def updateByKey[MT <: EntityModifier[MT]](modifier: MT, key: K, alias: Option[String] = None)(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKey[T, K, MT] = {
    UpdateAction.ByKey[T, K, MT](modifier, key)
  }

  final def updateByKeyReturningKeys[MT <: EntityModifier[MT]](modifier: MT, key: K, alias: Option[String] = None)(
      implicit
      tableSyntax: TableSyntax[T],
      tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKeyReturningKeys[T, K, MT] = {
    UpdateAction.ByKeyReturningKeys[T, K, MT](modifier, key)
  }

  // DELETE

  final def delete[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteAction.ByFilter[T, FT] = {
    val syntax = aliasedSyntax(alias)
    DeleteAction.ByFilter[T, FT](syntax, filter)
  }

  final def deleteReturningKeys[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteAction.ByFilterReturningKeys[T, K, FT] = {
    val syntax = aliasedSyntax(alias)
    DeleteAction.ByFilterReturningKeys[T, K, FT](syntax, filter)
  }

  final def deleteByKey(key: K, alias: Option[String] = None): DeleteAction.ByKey[T, K] = {
    val syntax = aliasedSyntax(alias)
    DeleteAction.ByKey[T, K](syntax, key)
  }

  final def deleteByKeyReturningKeys(key: K, alias: Option[String] = None): DeleteAction.ByKeyReturningKeys[T, K] = {
    val syntax = aliasedSyntax(alias)
    DeleteAction.ByKeyReturningKeys[T, K](syntax, key)
  }

  // WHERE

  final def combineFilterFragments[FT <: EntityFilter[FT]](filter: FT, alias: Option[String])(
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
        val recs = and.map(combineFilterFragments(_, alias))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      // OR between all Option[EntityFilter[T]] using self recursive type
      filter.OR.flatMap { or =>
        val recs = or.map(combineFilterFragments(_, alias))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      // NOT for one Option[EntityFilter[T]] using self recursive type
      filter.NOT.flatMap { not =>
        val rec = combineFilterFragments(not, alias)
        FragmentUtils.optionalNot(rec)
      }
    )
  }

  final def keyFilterFragment(key: K): Fragment = {
    keyWrite.toFragment(key, internalSyntax.keyColumns.map(col => s"$col = $Placeholder").mkString(s" $And "))
  }

  private def aliasedSyntax(alias: Option[String]): TableSyntax[T] = {
    alias.map(_ => table.internalSyntax).getOrElse(internalSyntax)
  }
}
