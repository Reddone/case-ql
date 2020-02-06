//package com.github.reddone.caseql.sql.generic
//
//import cats.data.NonEmptyList
//import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilter, LongFilter, StringFilter}
//import com.github.reddone.caseql.sql.generic.Table.Aux
//import com.github.reddone.caseql.sql.generic.TableFunction.extractFilter
//import doobie.Fragment
//import io.circe.Decoder
//import shapeless.labelled.{FieldType, field}
//import shapeless.{HList, LabelledGeneric, Lazy, Poly1, Witness, ops}
//
//final case class RelFilter2[A, T <: HasFilter[A, T]](
//    EVERY: Option[T],
//    SOME: Option[T],
//    NONE: Option[T]
//)
//
//object RelFilter2 {
//  implicit def decoder[A, T <: HasFilter[A, T]: Decoder]: Decoder[RelFilter2[A, T]] =
//    io.circe.generic.semiauto.deriveDecoder[RelFilter2[A, T]]
//}
//
//trait HasFilter[A, B <: HasFilter[A, B]] { self =>
//  def AND: Option[Seq[B]]
//  def OR: Option[Seq[B]]
//  def NOT: Option[B]
//}
//
//object extractRelationFilter2 extends TableFunction.extract[Option[RelFilter[_, _, _]]]
//
//trait relationFilterToFragment2[Tab] extends Poly1 {
//
//  implicit def atOptionRelFilter[A, T <: HasFilter[A, T], K <: Symbol, V <: Option[RelFilter2[A, T]]](
//      implicit
//      wt: Witness.Aux[K],
//      link: Link[Tab, A],
//      tableFilter: TableFilterRelV2[T]
//  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
//    at[FieldType[K, V]] { ft =>
//      //implicit val table = relation.tableB
//      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
//      field[K](
//        ft.flatMap(f =>
//          if (link.isJunction) {
//            // Single relation is implemented using a junction table
//            val left      = link.tableA.syntax("l")
//            val right     = link.tableB.syntax("r")
//            val middle    = link.tableC.syntax("m")
//            val leftCond  = link.leftJoinFields
//            val rightCond = link.rightJoinFields
//            // EVERY
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
//            // )
//
//            val leftCondSql = leftCond
//              .map {
//                case (lf, rf) => s"${left.column(lf)} = ${middle.column(rf)}"
//              }
//              .mkString(" AND ")
//            val rightCondSql = rightCond
//              .map {
//                case (lf, rf) => s"${right.column(lf)} = ${middle.column(rf)}"
//              }
//              .mkString(" AND ")
//            val rightIsNull = rightCond.map(_._1).map(r => s"IS NULL $r").mkString(" AND ")
//            val sqlString = (filterFrag: Fragment) =>
//              Fragment.const(s"""
//                                |NOT EXISTS (
//                                |SELECT ONE
//                                |FROM ${middle.name} LEFT JOIN ${right.name}
//                                |ON ${rightCondSql}
//                                |WHERE ${leftCondSql} AND $rightIsNull AND
//                                |""".stripMargin) ++ filterFrag ++
//                Fragment.const(")")
//
//            //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
//            // SOME
//            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of NONE
//            // EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id
//            // )
//
//            // NONE
//            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of SOME
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id
//            // )
//
//            None
//          } else {
//            // we have to always re-alias the right table because of self joins
//            // We can use the same syntax inside filters by wrapping everything inside a sub query
//
//            // Single relation is implemented using a direct table
//            val left     = link.tableA.defaultSyntax //.syntax("l")
//            val right    = link.tableB.defaultSyntax //.syntax("r")
//            val leftCond = link.leftJoinFields
//            val leftCondSql = leftCond
//              .map {
//                case (lf, rf) => s"${left.column(lf)} = ${right.column(rf)}"
//              }
//              .mkString(" AND ")
//            // EVERY
//            // EXISTS(
//            //  SELECT ONE WHERE
//            //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
//            //    as rightTable WHERE rightTable.id = leftTable.id)
//            //   =
//            //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id
//            // )
//
//            // SOME
//            // EXISTS(
//            //   SELECT ONE
//            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
//            //   WHERE rightTable.id = leftTable.id
//            // )
//            val sqlString = (filterFrag: Fragment) =>
//              Fragment.const(s"""
//                                |EXISTS (
//                                |SELECT ONE
//                                |FROM ${right.name}
//                                |WHERE ${leftCondSql} AND
//                                |""".stripMargin) ++ filterFrag ++
//                Fragment.const(")")
//
//            //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
//            // NONE
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
//            //   WHERE rightTable.id = leftTable.id
//            // )
//
//            None
//          }
//        )
//      )
//    }
//}
//
//object Test2 extends App {
//
//  case class A(field1: String, field2: Int)
//  case class AKey(field1: String)
//  case class B(field1: Long, field2: String)
//  case class BKey(field1: Long)
//
//  implicit val tableA: Aux[A, AKey] = Table.derive[A, AKey]()
//  implicit val tableB: Aux[B, BKey] = Table.derive[B, BKey]()
//
//  implicit val relAB: Link[A, B] =
//    Link.direct[A, B]((a, b) => NonEmptyList.of(("field1", "field2")))
//
//  case class AFilter(
//      field1: Option[StringFilter],
//      field2: Option[IntFilter],
//      bRelation: Option[RelFilter2[B, BFilter]]
//  ) extends HasFilter[A, AFilter] {
//    override def AND: Option[Seq[AFilter]] = None
//    override def OR: Option[Seq[AFilter]]  = None
//    override def NOT: Option[AFilter]      = None
//  }
//
//  implicit val decoder: Decoder[AFilter] = io.circe.generic.semiauto.deriveDecoder[AFilter]
//
//  case class BFilter(
//      field1: Option[LongFilter],
//      field2: Option[StringFilter]
//  ) extends HasFilter[B, BFilter] {
//    override def AND: Option[Seq[BFilter]] = None
//    override def OR: Option[Seq[BFilter]]  = None
//    override def NOT: Option[BFilter]      = None
//  }
//
//  implicit val decoder1: Decoder[BFilter] = io.circe.generic.semiauto.deriveDecoder[BFilter]
//
//  implicit val filter2: TableFilterRel[B, BFilter] = TableFilterRel.derive[B, BFilter]()
//  implicit val filter: TableFilterRel[A, AFilter]  = TableFilterRel.derive[A, AFilter]()
//
//  val bFilter = BFilter(
//    Some(LongFilter.empty.copy(EQ = Some(1L))),
//    Some(StringFilter.empty)
//  )
//  val aFilter = AFilter(
//    None,
//    None,
//    Some(RelFilter2(Some(bFilter), None, None))
//  )
//  println(
//    LabelledGeneric[AFilter]
//      .to(aFilter)
//      .flatMap(extractRelationFilter2)
//      .map(new relationFilterToFragment2[B] {})
//      .toList[Option[Fragment]]
//  )
//
//  val test = Derivator2[AFilter]().make.relationValues(LabelledGeneric[AFilter].to(aFilter))
//  println(test)
//}
//
//trait TableFilterRelV2[U] {
//  type T
//
//  def keys(): List[Symbol]
//  def values(u: U): List[Option[Filter[_]]]
//  def relations(u: U): List[Option[Fragment]]
//
//  def andCombinator(u: U): Option[Seq[U]] = None
//}
//
//object TableFilterRelV2 {
//
//  type Aux[U, T0] = TableFilterRelV2[U] { type T = T0 }
//
//  def apply[U](implicit ev: TableFilterRelV2[U]): Aux[U, ev.T] = ev
//
//  object derive {
//
//    def apply[T, U <: HasFilter[T, U]] = new Partial[T, U]
//
//    class Partial[T0, U] {
//
//      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
//          implicit
//          lgenT: LabelledGeneric.Aux[T0, L],
//          lgenU: LabelledGeneric.Aux[U, R],
//          tableFilterLR: Lazy[ReprTableFilterRelV2.Aux[T0, L, R, RKeys, RValues]],
//          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
//          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
//      ): Aux[U, T0] = new TableFilterRelV2[U] {
//        type T = T0
//        override def keys(): List[Symbol]                    = tableFilterLR.value.keys().toList
//        override def values(u: U): List[Option[Filter[_]]]   = tableFilterLR.value.values(lgenU.to(u)).toList
//        override def relations(u: U): List[Option[Fragment]] = tableFilterLR.value.relationValues(lgenU.to(u))
//      }
//    }
//  }
//}
//
//trait ReprTableFilterRelV2[T, L <: HList, R <: HList] {
//  type Keys <: HList
//  type Values <: HList
//
//  def keys(): Keys
//  def values(r: R): Values
//  def relationValues(r: R): List[Option[Fragment]]
//}
//
//object ReprTableFilterRelV2 {
//  type OptionFilter[A] = Option[Filter[A]]
//
//  type Aux[T, L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] =
//    ReprTableFilterRelV2[T, L, R] {
//      type Keys   = Keys0
//      type Values = Values0
//    }
//
//  //val and =
//
//  implicit def tableFilter[
//  T,
//      L <: HList,
//      LKeys <: HList,
//      LValues <: HList,
//      LValuesWrapped <: HList,
//      LZipped <: HList,
//      R <: HList,
//      RFilter <: HList,
//      RFilterKeys <: HList,
//      RFilterValues <: HList,
//      RAligned <: HList
//  ](
//      implicit
//      keysL: ops.record.Keys.Aux[L, LKeys],
//      valuesL: ops.record.Values.Aux[L, LValues],
//      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionFilter, LValuesWrapped],
//      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
//      extractFilterR: ops.hlist.FlatMapper.Aux[extractFilter.type, R, RFilter],
//      unzippedR: ops.record.UnzipFields.Aux[RFilter, RFilterKeys, RFilterValues],
//      alignR: ops.record.AlignByKeys.Aux[RFilter, LKeys, RAligned],
//      extTR: <:<[RAligned, LZipped],
//      rel: ReprRelationFilter2[T, R]
//  ): Aux[T, L, R, RFilterKeys, RFilterValues] = new ReprTableFilterRelV2[T, L, R] {
//    override type Keys   = RFilterKeys
//    override type Values = RFilterValues
//
//    override def keys(): RFilterKeys                          = unzippedR.keys()
//    override def values(r: R): RFilterValues                  = unzippedR.values(r.flatMap(extractFilter))
//    override def relationValues(r: R): List[Option[Fragment]] = rel.relationValues(r)
//  }
//}
//
//case class Derivator2[T]() {
//
//  implicit def make[L <: HList](
//      implicit
//      lgenT: LabelledGeneric.Aux[T, L],
//      rep: ReprRelationFilter2[T, L]
//  ): ReprRelationFilter2[T, L] = rep
//}
//
//trait ReprRelationFilter2[Tab, R <: HList] {
//  def relationValues(r: R): List[Option[Fragment]]
//}
//
//object ReprRelationFilter2 {
//
//  implicit def derive[
//  Tab,
//      R <: HList,
//      RRelFilter <: HList,
//      RRelMapped <: HList,
//  ](
//      implicit
//      extractRelFilterTR: ops.hlist.FlatMapper.Aux[extractRelationFilter2.type, R, RRelFilter],
//      mapper: ops.hlist.Mapper.Aux[relationFilterToFragment2[Tab] with Singleton, RRelFilter, RRelMapped],
//      toList: ops.hlist.ToList[RRelMapped, Option[Fragment]]
//  ): ReprRelationFilter2[Tab, R] = new ReprRelationFilter2[Tab, R] {
//    override def relationValues(r: R): List[Option[Fragment]] =
//      r.flatMap(extractRelationFilter2)(extractRelFilterTR)
//        .map(new relationFilterToFragment2[Tab] {})(mapper)
//        .toList(toList)
//  }
//}
