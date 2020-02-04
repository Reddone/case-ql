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
import io.circe.Decoder
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

//sealed trait RelFilter[A, B, T <: FilterWrapper[T]]
//
//final case class SingleRelFilter[A, B, T <: FilterWrapper[T]](
//    SOME: Option[T],
//    NONE: Option[T]
//) extends RelFilter[A, B, T]
//
//object SingleRelFilter {
//  implicit def decoder[A, B, T <: FilterWrapper[T]: Decoder]: Decoder[SingleRelFilter[A, B, T]] =
//    io.circe.generic.semiauto.deriveDecoder[SingleRelFilter[A, B, T]]
//}
//
//final case class MultiRelFilter[A, B, T <: FilterWrapper[T]](
//    EVERY: Option[T],
//    SOME: Option[T],
//    NONE: Option[T]
//) extends RelFilter[A, B, T]
//
//object MultiRelFilter {
//  implicit def decoder[A, B, T <: FilterWrapper[T]: Decoder]: Decoder[MultiRelFilter[A, B, T]] =
//    io.circe.generic.semiauto.deriveDecoder[MultiRelFilter[A, B, T]]
//}

final case class RelFilter[A, B, T <: FilterWrapper[T]](
    EVERY: Option[T],
    SOME: Option[T],
    NONE: Option[T]
)

object RelFilter {
  implicit def decoder[A, B, T <: FilterWrapper[T]: Decoder]: Decoder[RelFilter[A, B, T]] =
    io.circe.generic.semiauto.deriveDecoder[RelFilter[A, B, T]]
}

// link

trait Link[A, B] {
  type C

  def tableA: Table[A]
  def tableB: Table[B]
  def tableC: Table[C]
  def leftCondition: List[(String, String)]
  def rightCondition: List[(String, String)]
  def isJunction: Boolean
}

object Link {

  type Aux[A, B, C0] = Link[A, B] { type C = C0 }

  def direct[A, B](
      condition: (Table[A], Table[B]) => NonEmptyList[(String, String)]
  )(
      implicit
      tableAA: Table[A],
      tableBB: Table[B],
      tableCC: Table[Unit]
  ): Aux[A, B, Unit] = new Link[A, B] {
    override type C = Unit
    override def tableA: Table[A]                       = tableAA
    override def tableB: Table[B]                       = tableBB
    override def tableC: Table[C]                       = tableCC
    override def isJunction: Boolean                    = false
    override def leftCondition: List[(String, String)]  = condition(tableAA, tableBB).toList
    override def rightCondition: List[(String, String)] = List.empty
  }

  def junction[A, B, C0](
      leftCond: (Table[A], Table[C0]) => NonEmptyList[(String, String)],
      rightCond: (Table[B], Table[C0]) => NonEmptyList[(String, String)]
  )(
      implicit
      tableAA: Table[A],
      tableBB: Table[B],
      tableCC: Table[C0]
  ): Aux[A, B, C0] = new Link[A, B] {
    override type C = C0
    override def tableA: Table[A]                       = tableAA
    override def tableB: Table[B]                       = tableBB
    override def tableC: Table[C]                       = tableCC
    override def leftCondition: List[(String, String)]  = leftCond(tableAA, tableCC).toList
    override def rightCondition: List[(String, String)] = rightCond(tableBB, tableCC).toList
    override def isJunction: Boolean                    = true
  }

  implicit def inverse[A, B, C0](
      implicit link: Link.Aux[A, B, C0]
  ): Aux[B, A, C0] = new Link[B, A] {
    override type C = C0
    override def tableA: Table[B]                       = link.tableB
    override def tableB: Table[A]                       = link.tableA
    override def tableC: Table[C]                       = link.tableC
    override def leftCondition: List[(String, String)]  = link.rightCondition
    override def rightCondition: List[(String, String)] = link.leftCondition
    override def isJunction: Boolean                    = link.isJunction
  }
}

//final case class DirectLink[A, B](
//    //condition: (Table[A], Table[B]) => Seq[(String, String)]
//)(implicit tableAA: Table[A], tableBB: Table[B])
//    extends Link[A, B] {
//  type J = Unit
//  override def tableA: Table[A]            = tableAA
//  override def tableB: Table[B]            = tableBB
//  override def tableC: Option[Table[Unit]] = None
//}
//
//final case class JunctionLink[A, B](
//    //leftCondition: (Table[A], Table[Link[A, B]#J]) => Seq[(String, String)],
//    //rightCondition: (Table[B], Table[Link[A, B]#J]) => Seq[(String, String)]
//)(implicit tableAA: Table[A], tableBB: Table[B], tableC: Table[Link[A, B]#J])
//    extends Link[A, B] {
//  type J = Unit
//  override def tableA: Table[A]         = tableAA
//  override def tableB: Table[B]         = tableBB
//  override def tableC: Option[Table[J]] = Some(tableC)
//}

// relation

//sealed trait Relation[A, B] {
//  def toFragment: Fragment = Fragment.empty
//  def inverse: Relation[B, A]
//}
//
//object Relation {}
//
//trait SingleRelation[A, B] extends Relation[A, B]
//
//trait MultiRelation[A, B] extends Relation[A, B]
//
//case class OneToOneRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
//    implicit
//    tableA: Table[A],
//    tableB: Table[B]
//) extends SingleRelation[A, B] {
//  // JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
//  condition(tableA, tableB)
//    .map {
//      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
//    }
//    .toList
//    .mkString(" AND ")
//
//  override def inverse: Relation[B, A] = OneToOneRel[B, A]((b, a) => condition(a, b).map(_.swap))
//}
//
//case class OneToOneOptRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
//    implicit
//    tableA: Table[A],
//    tableB: Table[B]
//) extends SingleRelation[A, B] {
//  // LEFT JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
//  condition(tableA, tableB)
//    .map {
//      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
//    }
//    .toList
//    .mkString(" AND ")
//
//  override def inverse: Relation[B, A] = OneToOneOptRel[B, A]((b, a) => condition(a, b).map(_.swap))
//}
//
//case class ManyToOneRelation[A, B]() extends SingleRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class ManyToOneOptRelation[A, B]() extends SingleRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class OneToManyRelation[A, B]() extends MultiRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class OneToManyOptRelation[A, B]() extends MultiRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}

// extract all relation filters

object extractRelationFilter extends TableFunction.extract[Option[RelFilter[_, _, _]]]

// create the filter according to the relation type
// if it's a multiple we have to act in a different way than a single

object relationFilterToFragment extends Poly1 {
  implicit def atOptionSingleRelationFilter[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[
    RelFilter[A, B, T]
  ]](
      implicit
      wt: Witness.Aux[K],
      link: Link[A, B], // accept only single rel
      tableFilter: TableFilterRel[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      //implicit val table = relation.tableB
      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      field[K](ft.flatMap({
        case x: RelFilter[A, B, T] =>
          if (link.isJunction) {
            // Single relation is implemented using a junction table
            val nameLeft     = link.tableA.defaultSyntax.name
            val nameJunction = link.tableC.defaultSyntax.name
            val nameRight    = link.tableB.defaultSyntax.name
            // ONE

            // NONE

            None
          } else {
            // Single relation is implemented using a direct table

            // ONE

            // NONE

            None
          }
      }))
    }

  /*
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
        // EXISTS(SELECT ONE FROM rightTable WHERE leftID = rightID AND (filter))
        case rel: OneToOneOptRel[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
        // EXISTS(SELECT ONE FROM rightTable WHERE leftID = rightID AND (filter))
        case rel: ManyToOneRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
        // EXISTS(SELECT ONE FROM rightTable WHERE leftID = rightID AND (filter))
        case rel: ManyToOneOptRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
      }
    }

  implicit def atOptionMultiRelationFilter[A, B, T <: FilterWrapper[T], K <: Symbol, V <: Option[
    MultiRelFilter[A, B, T]
  ]](
      implicit
      wt: Witness.Aux[K],
      link: Link[A, B], // accept only multi relation
      tableB: Table[B],
      tableFilter: TableFilter[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      //implicit val table = relation.tableB
      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      link match {
        case x => ???
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

        //case rel: OneToManyRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))

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

        //case rel: OneToManyOptRelation[A, B] => field[K](ft.flatMap(f => None: Option[Fragment]))
      }
    }
 */
}

// test

object Test extends App {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  implicit val tableA: Aux[A, AKey] = Table.derive[A, AKey]()
  implicit val tableB: Aux[B, BKey] = Table.derive[B, BKey]()

  implicit val relAB: Link[A, B] =
    Link.direct[A, B]((a, b) => NonEmptyList.of((a.defaultSyntax.field1, b.defaultSyntax.field2)))

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      bRelation: Option[RelFilter[A, B, BFilter]]
  ) extends FilterWrapper[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }

  implicit val decoder: Decoder[AFilter] = io.circe.generic.semiauto.deriveDecoder[AFilter]

  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends FilterWrapper[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  implicit val decoder1: Decoder[BFilter] = io.circe.generic.semiauto.deriveDecoder[BFilter]

  implicit val filter2: TableFilterRel[B, BFilter] = TableFilterRel.derive[B, BFilter]()
  implicit val filter: TableFilterRel[A, AFilter]  = TableFilterRel.derive[A, AFilter]()

  val bFilter = BFilter(
    Some(LongFilter.empty.copy(EQ = Some(1L))),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(RelFilter[A, B, BFilter](Some(bFilter), None, None))
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

  println(implicitly[Table[Unit]])
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
          tableFilterLR: Lazy[ReprTableFilterRel.Aux[L, R, RKeys, RValues]],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
      ): TableFilterRel[T, U] = new TableFilterRel[T, U] {
        override def keys(): List[Symbol]                  = tableFilterLR.value.keys().toList
        override def values(u: U): List[Option[Filter[_]]] = tableFilterLR.value.values(lgenU.to(u)).toList
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
