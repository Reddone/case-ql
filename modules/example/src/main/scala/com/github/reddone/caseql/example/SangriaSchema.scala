package com.github.reddone.caseql.example

import cats.effect.Effect
import com.github.reddone.caseql.example.schema.{MutationDefinition, QueryDefinition}
import sangria.schema.{ObjectType, Schema}

object SangriaSchema {

  def apply[F[_]: Effect]: Schema[SangriaContext[F], Unit] =
    Schema(
      query = QueryType[F],
      mutation = Some(MutationType[F]),
      subscription = None,
      additionalTypes = Nil
    )

  def QueryType[F[_]: Effect]: ObjectType[SangriaContext[F], Unit] =
    ObjectType[SangriaContext[F], Unit]("Query", QueryDefinition[F])

  def MutationType[F[_]: Effect]: ObjectType[SangriaContext[F], Unit] =
    ObjectType[SangriaContext[F], Unit]("Mutation", MutationDefinition[F])
}
