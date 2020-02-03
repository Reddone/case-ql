package com.github.reddone.caseql.sql.generic

import java.io.FilterWriter

import cats.data.NonEmptyList
import cats.implicits._
import com.github.reddone.caseql.sql.filter.FilterWrapper
import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.generic.Table.{Aux, derive}
import com.github.reddone.caseql.sql.generic.TableFunction.extractFilter
import com.github.reddone.caseql.sql.modifier.models.Modifier
import doobie._
import doobie.implicits._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness, ops}
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HNil, Poly1, Witness}

// We have TableFilter[A, AA] and TableFilter[B, BB]

// 1) we can put RelationFilters inside filters and go unsafe
// 2) There is not way to keep track of relation between A and B inside filter

// a relation between A and B

// one to one        -> inner join    A has one B
// one to one opt    -> left join     A may have one B
// one to many       -> inner join    A has many B (fkey is on b)
// one to many opt   -> left join     A may have many B (fkey is on b)

trait Relation[A, B] {
  def toFragment: Fragment = Fragment.empty

  def inverse: Relation[B, A]
}

case class OneToOneRelation[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
    implicit
    tableA: Table[A],
    tableB: Table[B]
) extends Relation[A, B] {
  // JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
  condition(tableA, tableB)
    .map {
      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
    }
    .toList
    .mkString(" AND ")

  override def inverse: Relation[B, A] = OneToOneRelation[B, A]((b, a) => condition(a, b).map(_.swap))
}

case class OneToOneOptRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
    implicit
    tableA: Table[A],
    tableB: Table[B]
) extends Relation[A, B] {
  // LEFT JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
  condition(tableA, tableB)
    .map {
      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
    }
    .toList
    .mkString(" AND ")

  override def inverse: Relation[B, A] = OneToOneOptRel[B, A]((b, a) => condition(a, b).map(_.swap))
}

case class OneToManyRelation[A, B]() extends Relation[A, B] {
  override def inverse: Relation[B, A] = ???
}

case class OneToManyOptRelation[A, B, C]() extends Relation[A, B] {
  override def inverse: Relation[B, A] = ???
}

object Relation {}

// a filter using relations

trait RelFilter[A, B, T <: FilterWrapper[T]] {
  def filter: T
  def toOptionFragment: Option[Fragment]
}

final case class EveryRelFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

final case class SomeRelFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

final case class NoneRelFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

// 1) extract all RelationFilter
// 2) for each Filter, check that it exists a relation using a typeclass

trait HasRelation[R <: HList] {}

object HasRelation extends lowPriorityHasRelation {

  implicit def hconsHasRelation[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[RelFilter[A, B, T]], R <: HList](
      implicit
      witness: Witness.Aux[K],
      relation: Relation[A, B],
      tableFilter: TableFilter[B, T],
      hasRelation: HasRelation[R]
  ): HasRelation[FieldType[K, V] :: R] = new HasRelation[FieldType[K, V] :: R] {
    // here we use relation to create the appropriate join condition
    //FilterWrapper.filterFragment()
    //FilterWrapper.filterFragment()
  }
}

trait lowPriorityHasRelation {
  implicit def hnilHasRelation: HasRelation[HNil] = new HasRelation[HNil] {}
}

object extractRelationFilter extends TableFunction.extract[Option[RelFilter[_, _, _]]]

object relationFilterToFragment extends Poly1 {
  implicit def atOptionRelationFilter[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[RelFilter[A, B, T]]](
      implicit
      wt: Witness.Aux[K],
      relation: Relation[A, B],
      tableB: Table[B],
      tableFilter: TableFilter[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      //field[K](ft.flatMap(f => None: Option[Fragment]))
    }
}

// test

object Test extends App {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  implicit val tableA: Aux[A, AKey] = Table.derive[A, AKey]()
  implicit val tableB: Aux[B, BKey] = Table.derive[B, BKey]()

  implicit val relAB: Relation[A, B] = new Relation[A, B] {
    override def inverse: Relation[B, A] = ???
  }

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      everyB: Option[EveryRelFilter[A, B, BFilter]],
      someB: Option[SomeRelFilter[A, B, BFilter]]
  ) extends FilterWrapper[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }
  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends FilterWrapper[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  implicit val filter2: TableFilter[B, BFilter] = TableFilter.derive[B, BFilter]()
  implicit val filter: TableFilterRel[A, AFilter] = TableFilterRel.derive[A, AFilter]()


  val bFilter = BFilter(
    Some(LongFilter.empty.copy(EQ = Some(1L))),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(EveryRelFilter[A, B, BFilter](bFilter)),
    None
  )
  println(
    LabelledGeneric[AFilter]
      .to(aFilter)
      .flatMap(extractRelationFilter)
      .toList[Option[RelFilter[_, _, _]]]
  )
  println(
    LabelledGeneric[AFilter]
      .to(aFilter)
      .flatMap(extractRelationFilter)
      .map(relationFilterToFragment)
      .toList[Option[Fragment]]
  )

  val test = Derivator[AFilter]().make.relationValues(LabelledGeneric[AFilter].to(aFilter))
  println(test)
}

// alternative

trait TableFilterRel[T, U] {
  def keys(): List[Symbol]
  def values(u: U): List[Option[Filter[_]]]
}

object TableFilterRel {

  def apply[T, U](implicit ev: TableFilterRel[T, U]): TableFilterRel[T, U] = ev

  object derive {

    def apply[T, U] = new Partial[T, U]

    class Partial[T, U] {

      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
          implicit
          lgenT: LabelledGeneric.Aux[T, L],
          lgenU: LabelledGeneric.Aux[U, R],
          tableFilterLR: ReprTableFilterRel.Aux[L, R, RKeys, RValues],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
      ): TableFilterRel[T, U] = new TableFilterRel[T, U] {
        override def keys(): List[Symbol]                  = tableFilterLR.keys().toList
        override def values(u: U): List[Option[Filter[_]]] = tableFilterLR.values(lgenU.to(u)).toList
      }
    }
  }
}

trait ReprTableFilterRel[L <: HList, R <: HList] {
  type Keys <: HList
  type Values <: HList

  def keys(): Keys
  def values(r: R): Values
  def relationValues(r: R): List[Option[Fragment]]
}

object ReprTableFilterRel {
  type OptionFilter[A] = Option[Filter[A]]

  type Aux[L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] =
    ReprTableFilterRel[L, R] {
      type Keys   = Keys0
      type Values = Values0
    }

  implicit def tableFilter[
      L <: HList,
      LKeys <: HList,
      LValues <: HList,
      LValuesWrapped <: HList,
      LZipped <: HList,
      R <: HList,
      RFilter <: HList,
      RFilterKeys <: HList,
      RFilterValues <: HList,
      RAligned <: HList
  ](
      implicit
      keysL: ops.record.Keys.Aux[L, LKeys],
      valuesL: ops.record.Values.Aux[L, LValues],
      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionFilter, LValuesWrapped],
      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
      extractFilterR: ops.hlist.FlatMapper.Aux[extractFilter.type, R, RFilter],
      unzippedR: ops.record.UnzipFields.Aux[RFilter, RFilterKeys, RFilterValues],
      alignR: ops.record.AlignByKeys.Aux[RFilter, LKeys, RAligned],
      extTR: <:<[RAligned, LZipped],
      //hasRelations: HasRelation[RRelFilter],
      rel: ReprRelationFilter[R]
  ): Aux[L, R, RFilterKeys, RFilterValues] = new ReprTableFilterRel[L, R] {
    override type Keys   = RFilterKeys
    override type Values = RFilterValues

    override def keys(): RFilterKeys                          = unzippedR.keys()
    override def values(r: R): RFilterValues                  = unzippedR.values(r.flatMap(extractFilter))
    override def relationValues(r: R): List[Option[Fragment]] = rel.relationValues(r)
  }
}

case class Derivator[T]() {

  implicit def make[L <: HList](
      implicit
      lgenT: LabelledGeneric.Aux[T, L],
      rep: ReprRelationFilter[L]
  ): ReprRelationFilter[L] = rep
}

trait ReprRelationFilter[R <: HList] {
  def relationValues(r: R): List[Option[Fragment]]
}

object ReprRelationFilter {

  implicit def derive[
      R <: HList,
      RRelFilter <: HList,
      RRelMapped <: HList
  ](
      implicit
      extractRelFilterTR: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, R, RRelFilter],
      mapper: ops.hlist.Mapper.Aux[relationFilterToFragment.type, RRelFilter, RRelMapped],
      toList: ops.hlist.ToList[RRelMapped, Option[Fragment]]
  ): ReprRelationFilter[R] = new ReprRelationFilter[R] {
    override def relationValues(r: R): List[Option[Fragment]] =
      r.flatMap(extractRelationFilter)(extractRelFilterTR)
        .map(relationFilterToFragment)(mapper)
        .toList(toList)
  }
}
