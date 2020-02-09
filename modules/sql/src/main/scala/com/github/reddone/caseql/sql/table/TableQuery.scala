package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query.Query.{SQLAction, SQLStreamingAction}
import com.github.reddone.caseql.sql.table.query._
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie.Fragment

trait TableQuery[T, K] { table: Table[T, K] =>

  // SELECT

  final def select[FT <: EntityFilter[FT]](
      filter: FT,
      alias: Option[String] = None
  )(
      implicit tableFilter: TableFilter[T, FT]
  ): SQLStreamingAction[T] = {
    val builder = SelectBuilder
      .forTable(table, alias)
      .withFilter(filter)

    builder.buildSelect
  }

  final def selectByKey(
      key: K,
      alias: Option[String] = None
  ): SQLAction[Option[T]] = {
    val builder = SelectBuilder
      .forTable(table, alias)
      .withKey(key)

    builder.buildSelectByKey
  }

  // INSERT

  final def insertOne[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): SQLAction[Int] = {
    val builder = InsertBuilder
      .forTable(table)
      .withModifier(modifier)

    builder.buildInsertOne
  }

  final def insertOneReturningKey[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): SQLAction[K] = {
    val builder = InsertBuilder
      .forTable(table)
      .withModifier(modifier)

    builder.buildInsertOneReturningKey
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
  ): SQLAction[Int] = {
    val builder = UpdateBuilder
      .forTable(table, alias)
      .withModifier(modifier)
      .withFilter(filter)

    builder.buildUpdate
  }

  final def updateReturningKeys[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      modifier: MT,
      filter: FT,
      alias: Option[String] = None
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): SQLStreamingAction[K] = {
    val builder = UpdateBuilder
      .forTable(table, alias)
      .withModifier(modifier)
      .withFilter(filter)

    builder.buildUpdateReturningKeys
  }

  final def updateByKey[MT <: EntityModifier[MT]](
      modifier: MT,
      key: K,
      alias: Option[String] = None
  )(
      implicit tableModifier: TableModifier[T, MT]
  ): SQLAction[Int] = {
    val builder = UpdateBuilder
      .forTable(table, alias)
      .withModifier(modifier)
      .withKey(key)

    builder.buildUpdateByKey
  }

  final def updateByKeyReturningKeys[MT <: EntityModifier[MT]](
      modifier: MT,
      key: K,
      alias: Option[String] = None
  )(
      implicit tableModifier: TableModifier[T, MT]
  ): SQLStreamingAction[K] = {
    val builder = UpdateBuilder
      .forTable(table, alias)
      .withModifier(modifier)
      .withKey(key)

    builder.buildUpdateByKeyReturningKeys
  }

  // DELETE

  final def delete[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ) = {
    val builder = DeleteBuilder
      .forTable(table, alias)
      .withFilter(filter)

    builder.buildDelete
  }

  final def deleteReturningKeys[FT <: EntityFilter[FT]](filter: FT, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteQuery.ByFilterReturningKeys[T, K, FT] = {
    DeleteQuery.ByFilterReturningKeys(table, filter, alias)
  }

  final def deleteByKey(key: K, alias: Option[String] = None): DeleteQuery.ByKey[T, K] = {
    DeleteQuery.ByKey(table, key, alias)
  }

  final def deleteByKeyReturningKeys(key: K, alias: Option[String] = None): DeleteQuery.ByKeyReturningKeys[T, K] = {
    DeleteQuery.ByKeyReturningKeys(table, key, alias)
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
