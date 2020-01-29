package com.github.reddone.caseql.sql.util

import shapeless._
import shapeless.labelled.{FieldType, field}

// Excellent answer provided by Travis Brown:
// https://stackoverflow.com/questions/31640565/converting-mapstring-any-to-a-case-class
// The fix to work with options is provided here:
// https://stackoverflow.com/questions/55042252/shapeless-code-to-convert-mapstring-any-to-case-class-cannot-handle-optional
trait FromMap[L <: HList] {
  def apply(m: Map[String, Any]): Option[L]
}

trait LowPriorityFromMap {

  implicit def hconsFromMap1[K <: Symbol, V, T <: HList](
      implicit
      witness: Witness.Aux[K],
      typeable: Typeable[V],
      fromMapT: Lazy[FromMap[T]]
  ): FromMap[FieldType[K, V] :: T] = new FromMap[FieldType[K, V] :: T] {
    override def apply(m: Map[String, Any]): Option[FieldType[K, V] :: T] =
      for {
        v <- m.get(witness.value.name)
        h <- typeable.cast(v)
        t <- fromMapT.value(m)
      } yield field[K](h) :: t
  }
}

object FromMap extends LowPriorityFromMap {

  implicit val hnilFromMap: FromMap[HNil] = new FromMap[HNil] {
    override def apply(m: Map[String, Any]): Option[HNil] = Some(HNil)
  }

  implicit def hconsFromMap0[K <: Symbol, V, R <: HList, T <: HList](
      implicit
      witness: Witness.Aux[K],
      lgen: LabelledGeneric.Aux[V, R],
      fromMapH: FromMap[R],
      fromMapT: FromMap[T]
  ): FromMap[FieldType[K, V] :: T] = new FromMap[FieldType[K, V] :: T] {
    override def apply(m: Map[String, Any]): Option[FieldType[K, V] :: T] =
      for {
        v <- m.get(witness.value.name)
        r <- Typeable[Map[String, Any]].cast(v)
        h <- fromMapH(r)
        t <- fromMapT(m)
      } yield field[K](lgen.from(h)) :: t
  }

  implicit def hconsFromMap0opt[K <: Symbol, V, R <: HList, T <: HList](
      implicit
      witness: Witness.Aux[K],
      lgen: LabelledGeneric.Aux[V, R],
      fromMapH: FromMap[R],
      fromMapT: FromMap[T]
  ): FromMap[FieldType[K, Option[V]] :: T] = new FromMap[FieldType[K, Option[V]] :: T] {
    def apply(m: Map[String, Any]): Option[FieldType[K, Option[V]] :: T] =
      (for {
        v <- m.get(witness.value.name)
        r <- Typeable[Map[String, Any]].cast(v)
        h <- fromMapH(r)
        t <- fromMapT(m)
      } yield field[K](Some(lgen.from(h))) :: t).orElse(for {
        v  <- m.get(witness.value.name)
        r1 <- Typeable[Option[Map[String, Any]]].cast(v)
        opt = for {
          r <- r1
          h <- fromMapH(r)
        } yield lgen.from(h)
        t <- fromMapT(m)
      } yield field[K](opt) :: t)
  }
}

trait CaseClassFromMap[P <: Product] {
  def apply(m: Map[String, Any]): Option[P]
}

object CaseClassFromMap {

  def apply[P <: Product](map: Map[String, Any])(implicit fromMap: CaseClassFromMap[P]): P = fromMap(map).get

  implicit def mk[P <: Product, R <: HList](
      implicit
      lgen: LabelledGeneric.Aux[P, R],
      fromMap: FromMap[R]
  ): CaseClassFromMap[P] = new CaseClassFromMap[P] {
    def apply(m: Map[String, Any]): Option[P] = fromMap(m).map(lgen.from)
  }
}

object ConvertHelper {

  def to[A]: ConvertHelper[A] = new ConvertHelper[A]()
}

class ConvertHelper[A]() {

  def from[R <: HList](m: Map[String, Any])(
      implicit
      lgen: LabelledGeneric.Aux[A, R],
      fromMap: FromMap[R]
  ): Option[A] = {
    fromMap(m).map(lgen.from)
  }
}
