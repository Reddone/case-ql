package com.github.reddone.caseql.example

import cats.effect.Effect
import com.github.reddone.caseql.example.schema.QueryDefinition
import sangria.execution.deferred.{DeferredResolver, Fetcher}
import sangria.schema.{Field, ObjectType, Schema, StringType, fields}

object SangriaSchema {

  def schema[F[_]: Effect]: Schema[SangriaContext[F], Unit] = Schema(
    query = Query[F],
    mutation = Some(SangriaSchema.Mutation[F]),
    subscription = None,
    additionalTypes = Nil
  )

  def deferredResolver[F[_]: Effect]: DeferredResolver[SangriaContext[F]] = {
    val fetchers = Seq.empty[Fetcher[SangriaContext[F], _, _, _]]
    DeferredResolver.fetchers(fetchers: _*)
  }

  def Query[F[_]: Effect]: ObjectType[SangriaContext[F], Unit] =
    ObjectType[SangriaContext[F], Unit](
      "Query",
      commonQueryFields[SangriaContext[F]] ++ QueryDefinition.ExampleQuery[F])

  def Mutation[F[_]: Effect]: ObjectType[SangriaContext[F], Unit] =
    ObjectType[SangriaContext[F], Unit]("Mutation", Nil)

  private def commonQueryFields[Ctx]: List[Field[Ctx, Unit]] =
    fields[Ctx, Unit](
      Field(
        "liveness",
        StringType,
        Some("Liveness probe"),
        arguments = Nil,
        resolve = _ => "Hey there!"
      )
    )
}
