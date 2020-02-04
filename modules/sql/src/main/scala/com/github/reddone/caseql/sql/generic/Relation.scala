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

// relation filter
// we can create either a single relation filter or a multi rel filter

trait RelFilter[A, B, T <: FilterWrapper[T]]

final case class SingleRelFilter[A, B, T <: FilterWrapper[T]](
    ONE: Option[T],
    NONE: Option[T]
) extends RelFilter[A, B, T]

final case class MultiRelFilter[A, B, T <: FilterWrapper[T]](
    EVERY: Option[T],
    SOME: Option[T],
    NONE: Option[T]
) extends RelFilter[A, B, T]

// relation

sealed trait Relation[A, B] {
  def toFragment: Fragment = Fragment.empty
  def inverse: Relation[B, A]
}

object Relation {}

trait SingleRelation[A, B] extends Relation[A, B]

trait MultiRelation[A, B] extends Relation[A, B]

case class OneToOneRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
    implicit
    tableA: Table[A],
    tableB: Table[B]
) extends SingleRelation[A, B] {
  // JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
  condition(tableA, tableB)
    .map {
      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
    }
    .toList
    .mkString(" AND ")

  override def inverse: Relation[B, A] = OneToOneRel[B, A]((b, a) => condition(a, b).map(_.swap))
}

case class OneToOneOptRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
    implicit
    tableA: Table[A],
    tableB: Table[B]
) extends SingleRelation[A, B] {
  // LEFT JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
  condition(tableA, tableB)
    .map {
      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
    }
    .toList
    .mkString(" AND ")

  override def inverse: Relation[B, A] = OneToOneOptRel[B, A]((b, a) => condition(a, b).map(_.swap))
}

case class ManyToOneRelation[A, B]() extends SingleRelation[A, B] {
  override def inverse: Relation[B, A] = ???
}

case class ManyToOneOptRelation[A, B]() extends SingleRelation[A, B] {
  override def inverse: Relation[B, A] = ???
}

case class OneToManyRelation[A, B]() extends MultiRelation[A, B] {
  override def inverse: Relation[B, A] = ???
}

case class OneToManyOptRelation[A, B]() extends MultiRelation[A, B] {
  override def inverse: Relation[B, A] = ???
}

// extract all relation filters

object extractRelationFilter extends TableFunction.extract[Option[RelFilter[_, _, _]]]

// create the filter according to the relation type
// if it's a multiple we have to act in a different way than a single

object relationFilterToFragment extends Poly1 {
  implicit def atOptionSingleRelationFilter[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[
    SingleRelFilter[A, B, T]
  ]](
      implicit
      wt: Witness.Aux[K],
      relation: SingleRelation[A, B], // accept only single rel
      tableB: Table[B],
      tableFilter: TableFilter[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      //implicit val table = relation.tableB
      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      relation match {
        // EXISTS(SELECT ONE FROM rightTable WHERE leftTable.id = rightTable.id AND (filter))
        case rel: OneToOneRel[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
        // EXISTS(SELECT ONE FROM tableB WHERE leftID = rightID AND (filter))
        case rel: OneToOneOptRel[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
        // EXISTS(SELECT ONE FROM tableB WHERE leftID = rightID AND (filter))
        case rel: ManyToOneRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
        // EXISTS(SELECT ONE FROM tableB WHERE leftID = rightID AND (filter))
        case rel: ManyToOneOptRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
      }
    }

  implicit def atOptionMultiRelationFilter[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[
    MultiRelFilter[A, B, T]
  ]](
      implicit
      wt: Witness.Aux[K],
      relation: MultiRelation[A, B], // accept only multi relation
      tableB: Table[B],
      tableFilter: TableFilter[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      //implicit val table = relation.tableB
      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      relation match {
        // USING JOIN TABLE - EVERY
        // NOT EXISTS(
        //   SELECT ONE
        //   FROM joinTable
        //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
        //   ON jointable.id = rightTable.id
        //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
        // )
        // USING JOIN TABLE - SOME (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of NONE
        // EXISTS(
        //   SELECT ONE
        //   FROM joinTable
        //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
        //   ON jointable.id = rightTable.id
        //   WHERE joinTable.id = leftTable.id
        // )
        // USING JOIN TABLE - NONE (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of SOME
        // NOT EXISTS(
        //   SELECT ONE
        //   FROM joinTable
        //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
        //   ON jointable.id = rightTable.id
        //   WHERE joinTable.id = leftTable.id
        // )
        case rel: OneToManyRelation[A, B]    => field[K](ft.flatMap(f => None: Option[Fragment]))
        // USING FIELD ON OTHER ENTITY - EVERY
        // EXISTS(
        //  SELECT ONE WHERE
        //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
        //    as rightTable WHERE rightTable.id = leftTable.id)
        //   =
        //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id
        // )
        // USING FIELD ON OTHER ENTITY - SOME - Contrary of NONE
        // EXISTS(
        //   SELECT ONE
        //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
        //   WHERE rightTable.id = leftTable.id
        // )
        // USING FILED ON OTHER ENTITY - NONE - Contrary of SOME
        // NOT EXISTS(
        //   SELECT ONE
        //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
        //   WHERE rightTable.id = leftTable.id
        // )
        case rel: OneToManyOptRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
      }
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

  implicit val relAB: MultiRelation[A, B] = new MultiRelation[A, B] {
    override def inverse: Relation[B, A] = ???
  }

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      b: Option[MultiRelFilter[A, B, BFilter]]
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

  implicit val filter2: TableFilter[B, BFilter]   = TableFilter.derive[B, BFilter]()
  implicit val filter: TableFilterRel[A, AFilter] = TableFilterRel.derive[A, AFilter]()

  val bFilter = BFilter(
    Some(LongFilter.empty.copy(EQ = Some(1L))),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(MultiRelFilter[A, B, BFilter](Some(bFilter), None, None))
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

  def andCombinator(u: U): Option[Seq[U]] = None
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
