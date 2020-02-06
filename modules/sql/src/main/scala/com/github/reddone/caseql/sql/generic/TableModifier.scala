package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.generic.TableFunction._
import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableModifier[A, MA <: EntityModifier[A, MA]] {
  def entityModifierNamedFragments(modifier: MA): List[(String, Option[Fragment])]
}

object TableModifier {

  def apply[A, MA <: EntityModifier[A, MA]](implicit ev: TableModifier[A, MA]): TableModifier[A, MA] = ev

  object derive {

    def apply[A, MA <: EntityModifier[A, MA]] = new Partial[A, MA]

    class Partial[A, MA <: EntityModifier[A, MA]] {

      def apply[ReprA <: HList, ReprMA <: HList]()(
          implicit
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenMA: LabelledGeneric.Aux[MA, ReprMA],
          entityModifierMA: Lazy[ReprEntityModifier[A, ReprA, ReprMA]]
      ): TableModifier[A, MA] = new TableModifier[A, MA] {
        override def entityModifierNamedFragments(modifier: MA): List[(String, Option[Fragment])] = {
          entityModifierMA.value.entityModifierNamedFragments(lgenMA.to(modifier))
        }
      }
    }
  }
}

trait ReprEntityModifier[A, ReprA <: HList, ReprMA <: HList] {
  def entityModifierNamedFragments(modifierRepr: ReprMA): List[(String, Option[Fragment])]
}

object ReprEntityModifier {

  type OptionModifier[A] = Option[Modifier[A]]

  implicit def derive[
      A,
      ReprA <: HList,
      KeysA <: HList,
      ValuesA <: HList,
      WrappedValuesA <: HList,
      ZippedA <: HList,
      ReprMA <: HList,
      ModifierMA <: HList,
      NamedFragmentMA <: HList,
      AlignedModifierMA <: HList
  ](
      implicit
      tableA: Table[A],
      keysA: ops.record.Keys.Aux[ReprA, KeysA],
      valuesA: ops.record.Values.Aux[ReprA, ValuesA],
      wrappedValuesA: ops.hlist.Mapped.Aux[ValuesA, OptionModifier, WrappedValuesA],
      zippedA: ops.hlist.ZipWithKeys.Aux[KeysA, WrappedValuesA, ZippedA],
      filtersA: ops.hlist.FlatMapper.Aux[extractModifier.type, ReprMA, ModifierMA],
      namedFragmentsMA: ops.hlist.Mapper.Aux[modifierToNamedOptionFragment.type, ModifierMA, NamedFragmentMA],
      toListNamedFragmentsMA: ops.hlist.ToList[NamedFragmentMA, (String, Option[Fragment])],
      alignedMA: ops.record.AlignByKeys.Aux[ModifierMA, KeysA, AlignedModifierMA],
      isSubtypeMA: <:<[AlignedModifierMA, ZippedA]
  ): ReprEntityModifier[A, ReprA, ReprMA] = new ReprEntityModifier[A, ReprA, ReprMA] {
    override def entityModifierNamedFragments(modifierRepr: ReprMA): List[(String, Option[Fragment])] = {
      modifierRepr.flatMap(extractModifier).map(modifierToNamedOptionFragment).toList.map {
        case (name, fragment) =>
          val column = tableA.defaultSyntax.column(name)
          (column, fragment)
      }
    }
  }
}
