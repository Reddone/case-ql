package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import shapeless.labelled.FieldType
import shapeless.tag.@@
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, ops}

trait TableFilter[A, FA <: EntityFilter[FA]] {
  def primitiveFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]
  def relationFilterFragments(filter: FA): Option[String] => List[Option[Fragment]]

  final def byFilterFragment(filter: FA, alias: Option[String]): Option[Fragment] = {
    // AND between all possible filters
    FragmentUtils.optionalAndOpt(
      // AND between all Option[Filter[_]]
      FragmentUtils.optionalAndOpt(primitiveFilterFragments(filter)(alias): _*),
      // AND between all Option[RelationFilter[_, _, _]]
      FragmentUtils.optionalAndOpt(relationFilterFragments(filter)(alias): _*),
      // AND between all Option[EntityFilter[_]] using self recursive type
      filter.AND.flatMap { and =>
        val recs = and.map(byFilterFragment(_, alias))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      // OR between all Option[EntityFilter[_]] using self recursive type
      filter.OR.flatMap { or =>
        val recs = or.map(byFilterFragment(_, alias))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      // NOT for one Option[EntityFilter[_]] using self recursive type
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
          tableFilter: Lazy[ReprTableFilter[A, ReprA, ReprFA]]
      ): TableFilter[A, FA] = new TableFilter[A, FA] {
        override def primitiveFilterFragments(
            filter: FA
        ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
          tableFilter.value.primitiveFilterFragments(lgenFA.to(filter))(alias).reverse
        }

        override def relationFilterFragments(
            filter: FA
        ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
          tableFilter.value.relationFilterFragments(lgenFA.to(filter))(alias).reverse
        }
      }
    }
  }
}

trait ReprTableFilter[A, ReprA <: HList, ReprFA] {
  def primitiveFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
  def relationFilterFragments(filterRepr: ReprFA): Option[String] => List[Option[Fragment]]
}

object ReprTableFilter extends LowPriorityReprTableFilter {

  def pass: Option[String] => List[Option[Fragment]] = (_: Option[String]) => Nil

  implicit def hconsTableFilter[A, ReprA <: HList, K <: Symbol, V, T <: HList](
      implicit
      witness: Witness.Aux[K],
      headFilter: ReprTableFilter[A, ReprA, FieldType[K, V]],
      tailFilter: ReprTableFilter[A, ReprA, T]
  ): ReprTableFilter[A, ReprA, FieldType[K, V] :: T] =
    new ReprTableFilter[A, ReprA, FieldType[K, V] :: T] {
      override def primitiveFilterFragments(
          filterRepr: FieldType[K, V] :: T
      ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
        tailFilter.primitiveFilterFragments(filterRepr.tail)(alias) ++
          headFilter.primitiveFilterFragments(filterRepr.head)(alias)
      }

      override def relationFilterFragments(
          filterRepr: FieldType[K, V] :: T
      ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
        tailFilter.relationFilterFragments(filterRepr.tail)(alias) ++
          headFilter.relationFilterFragments(filterRepr.head)(alias)
      }
    }
}

trait LowPriorityReprTableFilter extends LowestPriorityReprTableFilter {

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
      ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
        List(filterRepr.flatMap(f => f.toOptionFragment(alias, tableSyntax, witness.value.name)))
      }

      override def relationFilterFragments(
          filterRepr: FieldType[K, V]
      ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass
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
      ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass

      override def relationFilterFragments(
          filterRepr: FieldType[K, V]
      ): Option[String] => List[Option[Fragment]] = { alias: Option[String] =>
        List(filterRepr.flatMap(f => f.toOptionFragment(alias, tableLink, tableFilter.value)))
      }
    }

  implicit def hnilTableFilter[A, ReprA <: HList]: ReprTableFilter[A, ReprA, HNil] =
    new ReprTableFilter[A, ReprA, HNil] {
      override def primitiveFilterFragments(
          filterRepr: HNil
      ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass

      override def relationFilterFragments(
          filterRepr: HNil
      ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass
    }
}

trait LowestPriorityReprTableFilter {

  type AND = Symbol @@ Witness.`"AND"`.T

  type OR = Symbol @@ Witness.`"OR"`.T

  type NOT = Symbol @@ Witness.`"NOT"`.T

  implicit def andTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< AND,
      valueEvidence: V <:< Option[Seq[EntityFilter[FA]]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = combinatorTableFilter[A, ReprA, K, V, FA]

  implicit def orTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< OR,
      valueEvidence: V <:< Option[Seq[EntityFilter[FA]]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = combinatorTableFilter[A, ReprA, K, V, FA]

  implicit def notTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]](
      implicit
      witness: Witness.Aux[K],
      keyEvidence: K <:< NOT,
      valueEvidence: V <:< Option[EntityFilter[FA]]
  ): ReprTableFilter[A, ReprA, FieldType[K, V]] = combinatorTableFilter[A, ReprA, K, V, FA]

  private def combinatorTableFilter[A, ReprA <: HList, K <: Symbol, V, FA <: EntityFilter[FA]]
      : ReprTableFilter[A, ReprA, FieldType[K, V]] = new ReprTableFilter[A, ReprA, FieldType[K, V]] {
    override def primitiveFilterFragments(
        filterRepr: FieldType[K, V]
    ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass

    override def relationFilterFragments(
        filterRepr: FieldType[K, V]
    ): Option[String] => List[Option[Fragment]] = ReprTableFilter.pass
  }
}
