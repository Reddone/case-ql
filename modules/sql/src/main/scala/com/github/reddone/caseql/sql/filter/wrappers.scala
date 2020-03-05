package com.github.reddone.caseql.sql.filter

import com.github.reddone.caseql.sql.table.TableFunction.{processDirectRelation, processJunctionRelation}
import com.github.reddone.caseql.sql.table.{TableFilter, TableLink}
import doobie.util.fragment.Fragment
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
  ) {
    def toOptionFragment(
        alias: Option[String],
        tableLink: TableLink[A, B],
        tableFilter: TableFilter[B, FB]
    ): Option[Fragment] = {
      val makeFragment =
        if (tableLink.isJunction) {
          processJunctionRelation(
            alias,
            tableLink.leftSyntax,
            tableLink.rightSyntax,
            tableLink.junctionSyntax,
            tableLink.leftJoinFields,
            tableLink.rightJoinFields,
            tableFilter
          )
        } else {
          processDirectRelation(
            alias,
            tableLink.leftSyntax,
            tableLink.rightSyntax,
            tableLink.leftJoinFields,
            tableFilter
          )
        }
      makeFragment(this)
    }
  }

  object RelationFilter {

    def empty[A, B, FB <: EntityFilter[FB]]: RelationFilter[A, B, FB] = RelationFilter[A, B, FB](None, None, None)

    def selfEmpty[A, FA <: EntityFilter[FA]]: RelationFilter[A, A, FA] = RelationFilter[A, A, FA](None, None, None)

    implicit def decoder[A, B, FB <: EntityFilter[FB]: Decoder]: Decoder[RelationFilter[A, B, FB]] =
      deriveDecoder[RelationFilter[A, B, FB]]
  }
}
