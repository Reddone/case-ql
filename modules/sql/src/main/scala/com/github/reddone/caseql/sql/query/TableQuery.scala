package com.github.reddone.caseql.sql.query

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.query.action.{DeleteAction, InsertAction, SelectAction, UpdateAction}
import doobie.util.Write
import shapeless.Typeable

import scala.reflect.ClassTag

trait TableQuery[T, K] { table: Table[T, K] =>

  // SELECT

  def select[FT <: EntityFilter[FT]](syntax: Syntax[T], filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): SelectAction.ByFilter[T, FT] = {
    SelectAction.ByFilter(syntax, filter)
  }

  def selectByKey(syntax: Syntax[T], key: K): SelectAction.ByKey[T, K] = {
    SelectAction.ByKey(syntax, key)
  }

  // INSERT

  def insertOne[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): InsertAction.One[T, MT] = {
    InsertAction.One(table.defaultSyntax, modifier)
  }

  def insertOneReturningKey[MT <: EntityModifier[MT]](modifier: MT)(
      implicit tableModifier: TableModifier[T, MT]
  ): InsertAction.OneReturningKey[T, K, MT] = {
    InsertAction.OneReturningKey(table.defaultSyntax, modifier)
  }

  // UPDATE

  def update[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      syntax: Syntax[T],
      modifier: MT,
      filter: FT
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilter[T, MT, FT] = {
    UpdateAction.ByFilter(syntax, modifier, filter)
  }

  def updateReturningKeys[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](
      syntax: Syntax[T],
      modifier: MT,
      filter: FT
  )(
      implicit
      tableModifier: TableModifier[T, MT],
      tableFilter: TableFilter[T, FT]
  ): UpdateAction.ByFilterReturningKeys[T, K, MT, FT] = {
    UpdateAction.ByFilterReturningKeys(syntax, modifier, filter)
  }

  def updateByKey[MT <: EntityModifier[MT]](syntax: Syntax[T], modifier: MT, key: K)(
      implicit tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKey[T, K, MT] = {
    UpdateAction.ByKey(syntax, modifier, key)
  }

  def updateByKeyReturningKeys[MT <: EntityModifier[MT]](syntax: Syntax[T], modifier: MT, key: K)(
      implicit tableModifier: TableModifier[T, MT]
  ): UpdateAction.ByKeyReturningKeys[T, K, MT] = {
    UpdateAction.ByKeyReturningKeys(syntax, modifier, key)
  }

  // DELETE

  def delete[FT <: EntityFilter[FT]](syntax: Syntax[T], filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): DeleteAction.ByFilter[T, FT] = {
    DeleteAction.ByFilter(syntax, filter)
  }

//  def deleteReturningKeys[FT <: EntityFilter[FT]](syntax: table.Syntax, filter: FT)(
//      implicit tableFilter: TableFilter[T, FT]
//  ): DeleteAction.ByFilterReturningKeys[T, FT, table.Key] = {
//    DeleteAction.ByFilterReturningKeys(syntax, filter)
//  }
//
//  def deleteByKey(syntax: table.Syntax, key: table.Key): DeleteAction.ByKey[T, table.Key] = {
//    DeleteAction.ByKey(syntax, key)
//  }
//
//  def deleteByKeyReturningKeys(
//      syntax: table.Syntax,
//      key: table.Key
//  )(): DeleteAction.ByKeyReturningKeys[T, table.Key] = {
//    DeleteAction.ByKeyReturningKeys(syntax, key)
//  }
}
