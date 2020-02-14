package com.github.reddone.caseql.sql.filter

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object wrappers {

  trait EntityFilter[FA <: EntityFilter[FA]] {
    def AND: Option[Seq[FA]]
    def OR: Option[Seq[FA]]
    def NOT: Option[FA]
  }

  final case class RelationFilter[A, B, FB <: EntityFilter[FB]](
      EVERY: Option[FB],
      SOME: Option[FB],
      NONE: Option[FB]
  )

  object RelationFilter {

    def empty[A, B, FB <: EntityFilter[FB]]: RelationFilter[A, B, FB] = RelationFilter[A, B, FB](None, None, None)

    implicit def decoder[A, B, FB <: EntityFilter[FB]: Decoder]: Decoder[RelationFilter[A, B, FB]] =
      deriveDecoder[RelationFilter[A, B, FB]]
  }
}
