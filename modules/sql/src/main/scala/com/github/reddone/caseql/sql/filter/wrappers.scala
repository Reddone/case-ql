package com.github.reddone.caseql.sql.filter

import com.github.reddone.caseql.sql.table.{RelationHelper, TableFilter, TableLink, TableSyntax}
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
        alias: Option[String],
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

  private def reAliasIfSelf[A, B](leftSyntax: TableSyntax[A], rightSyntax: TableSyntax[B]): TableSyntax[B] = {
    if (leftSyntax.aliasedName == rightSyntax.aliasedName) { // it's the same table
      if (rightSyntax.alias.isEmpty) {                       // and right part has no alias
        val newAlias = s"self"
        rightSyntax.withAlias("self")
      } else {
        val newAlias = s"${rightSyntax.alias}_self"          // or if right part has an alias
        rightSyntax.withAlias(newAlias)                      // append '_self' to existing alias
      }                                                      // it's not the same table
    } else {                                                 // keep using right alias
      rightSyntax                                            // because it's not a self join
    }
  }
}
