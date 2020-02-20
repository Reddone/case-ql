package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.TableFunction._
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableFilter[A, FA <: EntityFilter[FA]] {
  def entityFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]
  def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]

  final def byFilterFragment(filter: FA, alias: Option[String]): Option[Fragment] = {
    // AND between all possible filters
    FragmentUtils.optionalAndOpt(
      // AND between all Option[Filter[_]]
      FragmentUtils.optionalAndOpt(entityFilterFragments(filter)(alias): _*),
      // AND between all Option[RelationFilter[T, _, _]]
      FragmentUtils.optionalAndOpt(relationFilterFragments(filter)(alias): _*),
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
        FragmentUtils.optionalNotOpt(rec)
      }
    )
  }
}

object TableFilter {

  def apply[A, FA <: EntityFilter[FA]](implicit ev: TableFilter[A, FA]): TableFilter[A, FA] = ev

  object derive {

    def apply[A, FA <: EntityFilter[FA]] = new Partial[A, FA]

    class Partial[A, FA <: EntityFilter[FA]] {

      def apply[ReprA <: HList, ReprFA <: HList]()(
          implicit
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenFA: LabelledGeneric.Aux[FA, ReprFA],
          entityFilterFA: Lazy[ReprEntityFilter[A, ReprA, ReprFA]],
          relationFilterFA: Lazy[ReprRelationFilter[A, ReprFA]]
      ): TableFilter[A, FA] = new TableFilter[A, FA] {
        override def entityFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
          entityFilterFA.value.entityFilterFragments(lgenFA.to(filter))
        }
        override def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
          relationFilterFA.value.relationFilterFragments(lgenFA.to(filter))
        }
      }
    }
  }
}

trait ReprEntityFilter[A, ReprA <: HList, ReprFA <: HList] {
  def entityFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
}

object ReprEntityFilter {

  type OptionFilter[A] = Option[Filter[A]]

  implicit def derive[
      A,
      ReprA <: HList,
      KeysA <: HList,
      ValuesA <: HList,
      WrappedValuesA <: HList,
      ZippedA <: HList,
      ReprFA <: HList,
      FilterFA <: HList,
      FragmentFA <: HList,
      AlignedFilterFA <: HList
  ](
      implicit
      tableSyntaxA: TableSyntax[A],
      keysA: ops.record.Keys.Aux[ReprA, KeysA],
      valuesA: ops.record.Values.Aux[ReprA, ValuesA],
      wrappedValuesA: ops.hlist.Mapped.Aux[ValuesA, OptionFilter, WrappedValuesA],
      zippedA: ops.hlist.ZipWithKeys.Aux[KeysA, WrappedValuesA, ZippedA],
      filtersA: ops.hlist.FlatMapper.Aux[extractFilter.type, ReprFA, FilterFA],
      fragmentsFA: ops.hlist.Mapper.Aux[filterToNamedOptionFragment.type, FilterFA, FragmentFA],
      toListFragmentsFA: ops.hlist.ToList[FragmentFA, (String, String => Option[Fragment])],
      alignedFA: ops.record.AlignByKeys.Aux[FilterFA, KeysA, AlignedFilterFA],
      isSubtypeFA: <:<[AlignedFilterFA, ZippedA]
  ): ReprEntityFilter[A, ReprA, ReprFA] = new ReprEntityFilter[A, ReprA, ReprFA] {
    override def entityFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]] =
      (alias: Option[String]) => {
        val querySyntax = alias.map(tableSyntaxA.withAlias).getOrElse(tableSyntaxA)
        filterRepr
          .flatMap(extractFilter)
          .map(filterToNamedOptionFragment)
          .toList
          .map {
            case (field, makeFragment) =>
              val column   = querySyntax.aliasedColumn(field)
              val fragment = makeFragment(column)
              fragment
          }
      }
  }
}

trait ReprRelationFilter[A, ReprFA <: HList] {
  def relationFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
}

object ReprRelationFilter {

  implicit def derive[
      A,
      ReprFA <: HList,
      RelationFilterFA <: HList,
      FragmentFA <: HList
  ](
      implicit
      relationFiltersFA: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, ReprFA, RelationFilterFA],
      // TODO: use A to check that every RelationFilter is in the form RelationFilter[A, _, _]
      // TODO: B and FB inside RelationFilter can be anything, the mapper will validate them later
      fragmentsFA: ops.hlist.Mapper.Aux[relationFilterToOptionFragment.type, RelationFilterFA, FragmentFA],
      toListFragmentsFA: ops.hlist.ToList[FragmentFA, Option[String] => Option[Fragment]]
  ): ReprRelationFilter[A, ReprFA] = new ReprRelationFilter[A, ReprFA] {
    override def relationFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]] =
      (alias: Option[String]) =>
        filterRepr
          .flatMap(extractRelationFilter)
          .map(relationFilterToOptionFragment)
          .toList
          .map(_.apply(alias))
  }
}
