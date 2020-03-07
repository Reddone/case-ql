package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import doobie._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, ops}

trait TableModifier[A, MA <: EntityModifier[MA]] {
  def primitiveModifierNamedFragments(modifier: MA): List[Option[(String, Fragment)]]
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
          tableModifierMA: Lazy[ReprTableModifier[A, ReprA, ReprMA]]
      ): TableModifier[A, MA] = new TableModifier[A, MA] {
        override def primitiveModifierNamedFragments(modifier: MA): List[Option[(String, Fragment)]] = {
          tableModifierMA.value.primitiveModifierNamedFragments(lgenMA.to(modifier)).reverse
        }
      }
    }
  }
}

trait ReprTableModifier[A, ReprA <: HList, ReprMA] {
  def primitiveModifierNamedFragments(modifierRepr: ReprMA): List[Option[(String, Fragment)]]
}

object ReprTableModifier extends LowPriorityReprTableModifier {

  implicit def hconsTableModifier[A, ReprA <: HList, K <: Symbol, V, T <: HList](
      implicit
      witness: Witness.Aux[K],
      headModifier: ReprTableModifier[A, ReprA, FieldType[K, V]],
      tailModifier: ReprTableModifier[A, ReprA, T]
  ): ReprTableModifier[A, ReprA, FieldType[K, V] :: T] =
    new ReprTableModifier[A, ReprA, FieldType[K, V] :: T] {
      override def primitiveModifierNamedFragments(
          modifierRepr: FieldType[K, V] :: T
      ): List[Option[(String, Fragment)]] = {
        tailModifier.primitiveModifierNamedFragments(modifierRepr.tail) ++
          headModifier.primitiveModifierNamedFragments(modifierRepr.head)
      }
    }
}

trait LowPriorityReprTableModifier {

  implicit def primitiveTableModifier[A, ReprA <: HList, K <: Symbol, V, U, T](
      implicit
      witness: Witness.Aux[K],
      keySelector: ops.record.Selector.Aux[ReprA, K, U],
      modifierHasSameType: U =:= T,
      valueEvidence: V <:< Option[Modifier[T]],
      tableSyntax: TableSyntax[A]
  ): ReprTableModifier[A, ReprA, FieldType[K, V]] =
    new ReprTableModifier[A, ReprA, FieldType[K, V]] {
      override def primitiveModifierNamedFragments(
          modifierRepr: FieldType[K, V]
      ): List[Option[(String, Fragment)]] = {
        List(modifierRepr.map(m => m.toNamedFragment(tableSyntax, witness.value.name)))
      }
    }

  implicit def hnilTableModifier[A, ReprA <: HList]: ReprTableModifier[A, ReprA, HNil] =
    new ReprTableModifier[A, ReprA, HNil] {
      override def primitiveModifierNamedFragments(
          modifierRepr: HNil
      ): List[Option[(String, Fragment)]] = Nil
    }
}

//trait ReprEntityModifier[A, ReprA <: HList, ReprMA <: HList] {
//  def entityModifierNamedFragments(modifierRepr: ReprMA): List[(String, Option[Fragment])]
//}
//
//object ReprEntityModifier {
//
//  type OptionModifier[A] = Option[Modifier[A]]
//
//  implicit def derive[
//      A,
//      ReprA <: HList,
//      KeysA <: HList,
//      ValuesA <: HList,
//      WrappedValuesA <: HList,
//      ZippedA <: HList,
//      ReprMA <: HList,
//      ModifierMA <: HList,
//      NamedFragmentMA <: HList,
//      AlignedModifierMA <: HList
//  ](
//      implicit
//      tableSyntaxA: TableSyntax[A],
//      keysA: ops.record.Keys.Aux[ReprA, KeysA],
//      valuesA: ops.record.Values.Aux[ReprA, ValuesA],
//      wrappedValuesA: ops.hlist.Mapped.Aux[ValuesA, OptionModifier, WrappedValuesA],
//      zippedA: ops.hlist.ZipWithKeys.Aux[KeysA, WrappedValuesA, ZippedA],
//      modifiersA: ops.hlist.FlatMapper.Aux[extractModifier.type, ReprMA, ModifierMA],
//      namedFragmentsMA: ops.hlist.Mapper.Aux[modifierToNamedOptionFragment.type, ModifierMA, NamedFragmentMA],
//      toListNamedFragmentsMA: ops.hlist.ToList[NamedFragmentMA, (String, Option[Fragment])],
//      alignedMA: ops.record.AlignByKeys.Aux[ModifierMA, KeysA, AlignedModifierMA],
//      isSubtypeMA: <:<[AlignedModifierMA, ZippedA]
//  ): ReprEntityModifier[A, ReprA, ReprMA] = new ReprEntityModifier[A, ReprA, ReprMA] {
//    override def entityModifierNamedFragments(modifierRepr: ReprMA): List[(String, Option[Fragment])] =
//      modifierRepr.flatMap(extractModifier).map(modifierToNamedOptionFragment).toList.map {
//        case (name, fragment) =>
//          val column = tableSyntaxA.column(name)
//          (column, fragment)
//      }
//  }
//}
