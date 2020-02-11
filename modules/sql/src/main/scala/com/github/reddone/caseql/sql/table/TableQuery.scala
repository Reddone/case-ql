package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query._

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

  final def insert[MT <: EntityModifier[MT]](
      modifier: MT
  )(
      implicit tableModifier: TableModifier[T, MT]
  ): SQLAction[Int] = {
    val builder = InsertBuilder
      .forTable(table)
      .withModifier(modifier)

    builder.buildInsertOne
  }

  final def insertReturningKey[MT <: EntityModifier[MT]](
      modifier: MT
  )(
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

  final def delete[FT <: EntityFilter[FT]](
      filter: FT,
      alias: Option[String] = None
  )(
      implicit tableFilter: TableFilter[T, FT]
  ): SQLAction[Int] = {
    val builder = DeleteBuilder
      .forTable(table, alias)
      .withFilter(filter)

    builder.buildDelete
  }

  final def deleteReturningKeys[FT <: EntityFilter[FT]](
      filter: FT,
      alias: Option[String] = None
  )(
      implicit tableFilter: TableFilter[T, FT]
  ): SQLStreamingAction[K] = {
    val builder = DeleteBuilder
      .forTable(table, alias)
      .withFilter(filter)

    builder.buildDeleteReturningKeys
  }

  final def deleteByKey(
      key: K,
      alias: Option[String] = None
  ): SQLAction[Int] = {
    val builder = DeleteBuilder
      .forTable(table, alias)
      .withKey(key)

    builder.buildDeleteByKey
  }

  final def deleteByKeyReturningKeys(
      key: K,
      alias: Option[String] = None
  ): SQLStreamingAction[K] = {
    val builder = DeleteBuilder
      .forTable(table, alias)
      .withKey(key)

    builder.buildDeleteByKeyReturningKeys
  }
}
