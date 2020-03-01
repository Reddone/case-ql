package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.TableFunction._
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import shapeless.labelled.{FieldType, field}
import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness, ops}
import shapeless.::
import shapeless.tag.@@

trait TableFilter[A, FA <: EntityFilter[FA]] {
  def primitiveFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]
  def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]

  final def byFilterFragment(filter: FA, alias: Option[String]): Option[Fragment] = {
    // AND between all possible filters
    FragmentUtils.optionalAndOpt(
      // AND between all Option[Filter[_]]
      FragmentUtils.optionalAndOpt(primitiveFilterFragments(filter)(alias): _*),
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
          tableFilterFA: Lazy[ReprTableFilter[A, ReprA, ReprFA]]
      ): TableFilter[A, FA] = new TableFilter[A, FA] {
        override def primitiveFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
          alias: Option[String] =>
            tableFilterFA.value.primitiveFilterFragments(lgenFA.to(filter))(alias)
        }

        override def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
          (_: Option[String]) =>
            Nil
        }
      }
    }
  }
}

trait ReprTableFilter[A, ReprA <: HList, ReprFA] {

  def primitiveFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
}

object ReprTableFilter extends LowPriorityReprTableFilter {

  implicit def hlistTableFilter[A, ReprA <: HList, K <: Symbol, V, T <: HList](
      implicit
      witness: Witness.Aux[K],
      headFilter: ReprTableFilter[A, ReprA, FieldType[K, V]],
      tailFilter: ReprTableFilter[A, ReprA, T]
  ): ReprTableFilter[A, ReprA, FieldType[K, V] :: T] =
    new ReprTableFilter[A, ReprA, FieldType[K, V] :: T] {
      override def primitiveFilterFragments(
          filterRepr: FieldType[K, V] :: T
      ): Option[String] => List[Option[Fragment]] = {
        println("DERIVE")
        (alias: Option[String]) =>
          headFilter.primitiveFilterFragments(filterRepr.head)(alias) :::
            tailFilter.primitiveFilterFragments(filterRepr.tail)(alias)
      }
    }

}

trait LowPriorityReprTableFilter extends LowestPriorityReprTableFilter {

//  implicit def primitiveFieldFilter[A, ReprA <: HList, K <: Symbol, V]
//  //(
//  //implicit
//  // TODO: type check primitive filter
//  //ev: V <:< Option[Filter[B]],
//  // witness: Witness.Aux[K]
//  // tableSyntax: TableSyntax[A]
//  //)
//      : ReprTableFilter[A, ReprA, FieldType[K, V]] =
//    new ReprTableFilter[A, ReprA, FieldType[K, V]] {
//      override def primitiveFilterFragments(
//          field: FieldType[K, V]
//      ): Option[String] => List[Option[Fragment]] = {
//        println("PRIMITIVE")
//        //val name = witness.value.name
//        //alias: Option[String] => {
//        //  val querySyntax = alias.map(tableSyntax.withAlias).getOrElse(tableSyntax)
//        //  List(field.flatMap(f => f.toOptionFragment(querySyntax.aliasedColumn(name))))
//        (_: Option[String]) => Nil
//      }
//    }

  implicit def primitiveTableFilter[A, ReprA <: HList, K <: Symbol, V, U, T](
      implicit
      witness: Witness.Aux[K],
      keySelector: ops.record.Selector.Aux[ReprA, K, U],
      filterHasSameType: U =:= T,
      valueEvidence: V <:< Option[Filter[T]],
      tableSyntax: TableSyntax[A]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] =
    new ReprTableFilter[A, ReprA, FieldType[K, V]] {
      override def primitiveFilterFragments(
          filterRepr: FieldType[K, V]
      ): Option[String] => List[Option[Fragment]] = {
        println("PRIMITIVE")
        val name = witness.value.name
        alias: Option[String] => {
          val querySyntax = alias.map(tableSyntax.withAlias).getOrElse(tableSyntax)
          List(filterRepr.flatMap(f => f.toOptionFragment(querySyntax.aliasedColumn(name))))
        }
      }
    }

  implicit def relationTableFilter[A, ReprA <: HList, K <: Symbol, V, B, FB <: EntityFilter[FB]](
      implicit
      witness: Witness.Aux[K],
      valueEvidence: V <:< Option[RelationFilter[A, B, FB]],
      tableLink: TableLink[A, B],
      tableFilter: Lazy[TableFilter[B, FB]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] =
    new ReprTableFilter[A, ReprA, FieldType[K, V]] {
      override def primitiveFilterFragments(
          filterRepr: FieldType[K, V]
      ): Option[String] => List[Option[Fragment]] = {
        println("RELATION")
        (_: Option[String]) => Nil
      }
    }

  implicit def hnilTableFilter[A, ReprA <: HList]: ReprTableFilter[A, ReprA, HNil] =
    new ReprTableFilter[A, ReprA, HNil] {
      override def primitiveFilterFragments(filterRepr: HNil): Option[String] => List[Option[Fragment]] = {
        println("END")
        (_: Option[String]) => Nil
      }
    }

}

trait LowestPriorityReprTableFilter extends ZeroPriorityReprTableFilter {

  type AND = Symbol @@ Witness.`"AND"`.T

  implicit def andTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< AND,
      valueEvidence: V <:< Option[Seq[EntityFilter[FA]]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = new ReprTableFilter[A, ReprA, FieldType[K, V]] {
    override def primitiveFilterFragments(
        filterRepr: FieldType[K, V]
    ): Option[String] => List[Option[Fragment]] = {
      println("AND")
      (alias: Option[String]) => List.empty[Option[Fragment]]
    }
  }

  type OR = Symbol @@ Witness.`"OR"`.T

  implicit def orTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< OR,
      valueEvidence: V <:< Option[Seq[EntityFilter[FA]]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = new ReprTableFilter[A, ReprA, FieldType[K, V]] {
    override def primitiveFilterFragments(
        filterRepr: FieldType[K, V]
    ): Option[String] => List[Option[Fragment]] = {
      println("OR")
      (alias: Option[String]) => List.empty[Option[Fragment]]
    }
  }

  type NOT = Symbol @@ Witness.`"NOT"`.T

  implicit def notTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< NOT,
      valueEvidence: V <:< Option[EntityFilter[FA]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = new ReprTableFilter[A, ReprA, FieldType[K, V]] {
    override def primitiveFilterFragments(
        filterRepr: FieldType[K, V]
    ): Option[String] => List[Option[Fragment]] = {
      println("NOT")
      (alias: Option[String]) => List.empty[Option[Fragment]]
    }
  }
}

trait ZeroPriorityReprTableFilter {

  def skipTableFilter[A, ReprA <: HList, K <: Symbol, V]: ReprTableFilter[A, ReprA, FieldType[K, V]] =
    new ReprTableFilter[A, ReprA, FieldType[K, V]] {
      override def primitiveFilterFragments(
          filterRepr: FieldType[K, V]
      ): Option[String] => List[Option[Fragment]] = {
        println("SKIP")
        (alias: Option[String]) => List.empty[Option[Fragment]]
      }
    }
}
//object TableFilter {
//
//  def apply[A, FA <: EntityFilter[FA]](implicit ev: TableFilter[A, FA]): TableFilter[A, FA] = ev
//
//  object derive {
//
//    def apply[A, FA <: EntityFilter[FA]] = new Partial[A, FA]
//
//    class Partial[A, FA <: EntityFilter[FA]] {
//
//      def apply[ReprA <: HList, ReprFA <: HList]()(
//          implicit
//          lgenA: LabelledGeneric.Aux[A, ReprA],
//          lgenFA: LabelledGeneric.Aux[FA, ReprFA],
//          entityFilterFA: Lazy[ReprEntityFilter[A, ReprA, ReprFA]],
//          relationFilterFA: Lazy[ReprRelationFilter[A, ReprFA]]
//      ): TableFilter[A, FA] = new TableFilter[A, FA] {
//        override def primitiveFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
//          entityFilterFA.value.primitiveFilterFragments(lgenFA.to(filter))
//        }
//        override def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]] = {
//          relationFilterFA.value.relationFilterFragments(lgenFA.to(filter))
//        }
//      }
//    }
//  }
//}
//
//trait ReprEntityFilter[A, ReprA <: HList, ReprFA <: HList] {
//  def primitiveFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
//}
//
//object ReprEntityFilter {
//
//  type OptionFilter[A] = Option[Filter[A]]
//
//  implicit def derive[
//      A,
//      ReprA <: HList,
//      KeysA <: HList,
//      ValuesA <: HList,
//      WrappedValuesA <: HList,
//      ZippedA <: HList,
//      ReprFA <: HList,
//      FilterFA <: HList,
//      FragmentFA <: HList,
//      AlignedFilterFA <: HList
//  ](
//      implicit
//      tableSyntaxA: TableSyntax[A],
//      keysA: ops.record.Keys.Aux[ReprA, KeysA],
//      valuesA: ops.record.Values.Aux[ReprA, ValuesA],
//      wrappedValuesA: ops.hlist.Mapped.Aux[ValuesA, OptionFilter, WrappedValuesA],
//      zippedA: ops.hlist.ZipWithKeys.Aux[KeysA, WrappedValuesA, ZippedA],
//      filtersA: ops.hlist.FlatMapper.Aux[extractFilter.type, ReprFA, FilterFA],
//      fragmentsFA: ops.hlist.Mapper.Aux[filterToNamedOptionFragment.type, FilterFA, FragmentFA],
//      toListFragmentsFA: ops.hlist.ToList[FragmentFA, (String, String => Option[Fragment])],
//      alignedFA: ops.record.AlignByKeys.Aux[FilterFA, KeysA, AlignedFilterFA],
//      isSubtypeFA: <:<[AlignedFilterFA, ZippedA]
//  ): ReprEntityFilter[A, ReprA, ReprFA] = new ReprEntityFilter[A, ReprA, ReprFA] {
//    override def primitiveFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]] =
//      (alias: Option[String]) => {
//        val querySyntax = alias.map(tableSyntaxA.withAlias).getOrElse(tableSyntaxA)
//        filterRepr
//          .flatMap(extractFilter)
//          .map(filterToNamedOptionFragment)
//          .toList
//          .map {
//            case (field, makeFragment) =>
//              val column   = querySyntax.aliasedColumn(field)
//              val fragment = makeFragment(column)
//              fragment
//          }
//      }
//  }
//}
//
//trait ReprRelationFilter[A, ReprFA <: HList] {
//  def relationFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
//}
//
//object ReprRelationFilter {
//
//  implicit def derive[
//      A,
//      ReprFA <: HList,
//      RelationFilterFA <: HList,
//      FragmentFA <: HList
//  ](
//      implicit
//      relationFiltersFA: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, ReprFA, RelationFilterFA],
//      // TODO: use A to check that every RelationFilter is in the form RelationFilter[A, _, _]
//      // TODO: B and FB inside RelationFilter can be anything, the mapper will validate them later
//      fragmentsFA: ops.hlist.Mapper.Aux[relationFilterToOptionFragment.type, RelationFilterFA, FragmentFA],
//      toListFragmentsFA: ops.hlist.ToList[FragmentFA, Option[String] => Option[Fragment]]
//  ): ReprRelationFilter[A, ReprFA] = new ReprRelationFilter[A, ReprFA] {
//    override def relationFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]] =
//      (alias: Option[String]) =>
//        filterRepr
//          .flatMap(extractRelationFilter)
//          .map(relationFilterToOptionFragment)
//          .toList
//          .map(_.apply(alias))
//  }
//}
