package com.github.reddone.caseql.sql.filter

import com.github.reddone.caseql.sql.table.{RelationHelper, TableFilter, TableLink}
import doobie.util.fragment.Fragment

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
        alias: String,
        tableLink: TableLink[A, B],
        tableFilter: TableFilter[B, FB]
    ): Option[Fragment] = {
      val makeFragment =
        if (tableLink.isJunction) {
          RelationHelper.processJunctionRelationFilter(
            alias,
            tableLink.leftSyntax,
            tableLink.rightSyntax,
            tableLink.junctionSyntax,
            tableLink.leftJoinFields,
            tableLink.rightJoinFields,
            tableFilter
          )
        } else {
          RelationHelper.processDirectRelationFilter(
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
  }
}
