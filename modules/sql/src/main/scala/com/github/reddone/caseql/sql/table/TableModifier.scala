package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import com.github.reddone.caseql.sql.table.TableFunction._
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableModifier[A, MA <: EntityModifier[MA]] {
  def entityModifierNamedFragments(modifier: MA): Option[String] => List[(String, Option[Fragment])]
}

object TableModifier {

  def apply[A, MA <: EntityModifier[MA]](implicit ev: TableModifier[A, MA]): TableModifier[A, MA] = ev

  object derive {

    def apply[A, MA <: EntityModifier[MA]] = new Partial[A, MA]

    class Partial[A, MA <: EntityModifier[MA]] {

      def apply[ReprA <: HList, ReprMA <: HList]()(
          implicit
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenMA: LabelledGeneric.Aux[MA, ReprMA],
          entityModifierMA: Lazy[ReprEntityModifier[A, ReprA, ReprMA]]
      ): TableModifier[A, MA] = new TableModifier[A, MA] {
        override def entityModifierNamedFragments(modifier: MA): Option[String] => List[(String, Option[Fragment])] = {
          entityModifierMA.value.entityModifierNamedFragments(lgenMA.to(modifier))
        }
      }
    }
  }
}

trait ReprEntityModifier[A, ReprA <: HList, ReprMA <: HList] {
  def entityModifierNamedFragments(modifierRepr: ReprMA): Option[String] => List[(String, Option[Fragment])]
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
      tableSyntaxA: TableSyntax[A],
      keysA: ops.record.Keys.Aux[ReprA, KeysA],
      valuesA: ops.record.Values.Aux[ReprA, ValuesA],
      wrappedValuesA: ops.hlist.Mapped.Aux[ValuesA, OptionModifier, WrappedValuesA],
      zippedA: ops.hlist.ZipWithKeys.Aux[KeysA, WrappedValuesA, ZippedA],
      modifiersA: ops.hlist.FlatMapper.Aux[extractModifier.type, ReprMA, ModifierMA],
      namedFragmentsMA: ops.hlist.Mapper.Aux[modifierToNamedOptionFragment.type, ModifierMA, NamedFragmentMA],
      toListNamedFragmentsMA: ops.hlist.ToList[NamedFragmentMA, (String, Option[Fragment])],
      alignedMA: ops.record.AlignByKeys.Aux[ModifierMA, KeysA, AlignedModifierMA],
      isSubtypeMA: <:<[AlignedModifierMA, ZippedA]
  ): ReprEntityModifier[A, ReprA, ReprMA] = new ReprEntityModifier[A, ReprA, ReprMA] {
    override def entityModifierNamedFragments(
        modifierRepr: ReprMA
    ): Option[String] => List[(String, Option[Fragment])] =
      (alias: Option[String]) => {
        val modifierSyntax = tableSyntaxA.withAlias(alias.getOrElse(""))
        modifierRepr.flatMap(extractModifier).map(modifierToNamedOptionFragment).toList.map {
          case (name, fragment) =>
            val column = modifierSyntax.column(name)
            (column, fragment)
        }
      }
  }
}
