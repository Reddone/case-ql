package com.github.reddone.caseql.sql.query

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.query.TableFunction._
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableFilter[A, FA <: EntityFilter[FA]] {
  def entityFilterFragments(filter: FA): Syntax[A] => List[Option[Fragment]]
  def relationFilterFragments(filter: FA): Syntax[A] => List[Option[Fragment]]
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
        override def entityFilterFragments(filter: FA): Syntax[A] => List[Option[Fragment]] = {
          entityFilterFA.value.entityFilterFragments(lgenFA.to(filter))
        }
        override def relationFilterFragments(filter: FA): Syntax[A] => List[Option[Fragment]] = {
          relationFilterFA.value.relationFilterFragments(lgenFA.to(filter))
        }
      }
    }
  }
}

trait ReprEntityFilter[A, ReprA <: HList, ReprFA <: HList] {
  def entityFilterFragments(filterRepr: ReprFA): Syntax[A] => List[Option[Fragment]]
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
    override def entityFilterFragments(filterRepr: ReprFA): Syntax[A] => List[Option[Fragment]] = {
      syntax: Syntax[A] =>
        filterRepr
          .flatMap(extractFilter)
          .map(filterToNamedOptionFragment)
          .toList
          .map {
            case (field, makeFragment) =>
              val column   = syntax.column(field)
              val fragment = makeFragment(column)
              fragment
          }
    }
  }
}

trait ReprRelationFilter[A, ReprFA <: HList] {
  def relationFilterFragments(filterRepr: ReprFA): Syntax[A] => List[Option[Fragment]]
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
      //belongsToT: ops.record.Values.Aux[RelationFilterFA, OUT],
      //aaaa: ops.hlist.Comapped[OUT, Option[RelationFilter[A, _, _]],
      fragmentsFA: ops.hlist.Mapper.Aux[relationFilterToOptionFragment.type, RelationFilterFA, FragmentFA],
      toListFragmentsFA: ops.hlist.ToList[FragmentFA, Syntax[A] => Option[Fragment]]
  ): ReprRelationFilter[A, ReprFA] = new ReprRelationFilter[A, ReprFA] {
    override def relationFilterFragments(filterRepr: ReprFA): Syntax[A] => List[Option[Fragment]] = {
      syntax: Syntax[A] =>
        filterRepr
          .flatMap(extractRelationFilter)
          .map(relationFilterToOptionFragment)
          .toList
          .map(_.apply(syntax))
    }
  }
}
