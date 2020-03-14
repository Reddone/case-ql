package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.query._

trait TableQuery[A, K] { table: Table[A, K] =>

  // SELECT

  final def select[FA <: EntityFilter[FA]](filter: FA, alias: Option[String] = None)(
      implicit tableFilter: TableFilter[A, FA]
  ): SQLStreamingAction[A] = {
    val builder = SelectBuilder
      .forTable(table, alias)
      .withFilter(filter)

    builder.buildSelect
  }

  final def selectByKey(key: K, alias: Option[String] = None): SQLAction[Option[A]] = {
    val builder = SelectBuilder
      .forTable(table, alias)
      .withKey(key)

    builder.buildSelectByKey
  }

  // INSERT

  final def insert[MA <: EntityModifier[MA]](modifier: MA)(
      implicit tableModifier: TableModifier[A, MA]
  ): SQLAction[Int] = {
    val builder = InsertBuilder
      .forTable(table)
      .withModifier(modifier)

    builder.buildInsertOne
  }

  final def insertReturningKey[MA <: EntityModifier[MA]](modifier: MA)(
      implicit tableModifier: TableModifier[A, MA]
  ): SQLAction[K] = {
    val builder = InsertBuilder
      .forTable(table)
      .withModifier(modifier)

    builder.buildInsertOneReturningKey
  }

  // UPDATE

  final def update[MA <: EntityModifier[MA], FA <: EntityFilter[FA]](modifier: MA, filter: FA)(
      implicit
      tableModifier: TableModifier[A, MA],
      tableFilter: TableFilter[A, FA]
  ): SQLAction[Int] = {
    val builder = UpdateBuilder
      .forTable(table)
      .withModifier(modifier)
      .withFilter(filter)

    builder.buildUpdate
  }

  final def updateReturningKeys[MA <: EntityModifier[MA], FA <: EntityFilter[FA]](modifier: MA, filter: FA)(
      implicit
      tableModifier: TableModifier[A, MA],
      tableFilter: TableFilter[A, FA]
  ): SQLStreamingAction[K] = {
    val builder = UpdateBuilder
      .forTable(table)
      .withModifier(modifier)
      .withFilter(filter)

    builder.buildUpdateReturningKeys
  }

  final def updateByKey[MA <: EntityModifier[MA]](modifier: MA, key: K)(
      implicit tableModifier: TableModifier[A, MA]
  ): SQLAction[Int] = {
    val builder = UpdateBuilder
      .forTable(table)
      .withModifier(modifier)
      .withKey(key)

    builder.buildUpdateByKey
  }

  final def updateByKeyReturningKeys[MA <: EntityModifier[MA]](modifier: MA, key: K)(
      implicit tableModifier: TableModifier[A, MA]
  ): SQLStreamingAction[K] = {
    val builder = UpdateBuilder
      .forTable(table)
      .withModifier(modifier)
      .withKey(key)

    builder.buildUpdateByKeyReturningKeys
  }

  // DELETE

  final def delete[FA <: EntityFilter[FA]](filter: FA)(
      implicit tableFilter: TableFilter[A, FA]
  ): SQLAction[Int] = {
    val builder = DeleteBuilder
      .forTable(table)
      .withFilter(filter)

    builder.buildDelete
  }

  final def deleteReturningKeys[FA <: EntityFilter[FA]](filter: FA)(
      implicit tableFilter: TableFilter[A, FA]
  ): SQLStreamingAction[K] = {
    val builder = DeleteBuilder
      .forTable(table)
      .withFilter(filter)

    builder.buildDeleteReturningKeys
  }

  final def deleteByKey(key: K): SQLAction[Int] = {
    val builder = DeleteBuilder
      .forTable(table)
      .withKey(key)

    builder.buildDeleteByKey
  }

  final def deleteByKeyReturningKeys(key: K): SQLStreamingAction[K] = {
    val builder = DeleteBuilder
      .forTable(table)
      .withKey(key)

    builder.buildDeleteByKeyReturningKeys
  }
}
