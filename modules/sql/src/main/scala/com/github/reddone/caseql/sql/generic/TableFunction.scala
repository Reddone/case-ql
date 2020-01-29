package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.modifier.models.Modifier
import doobie._
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HNil, Poly1, Witness}

object TableFunction {

  // type extractor

  object extractFilter extends extract[Option[Filter[_]]]

  object extractModifier extends extract[Option[Modifier[_]]]

  trait extract[T] extends skip {
    implicit def atType[K, V <: T](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, V] :: HNil] =
      at[FieldType[K, V]](_ :: HNil)
  }

  trait skip extends Poly1 {
    implicit def atDefault[T]: Case.Aux[T, HNil] = at[T](_ => HNil)
  }

  // filter to option fragment

  object filterToOptionFragment extends Poly1 {
    implicit def atOptionFilter[K, V <: Option[Filter[_]]](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, String => Option[Fragment]]] =
      at[FieldType[K, V]] { ft =>
        field[K](ft.map(f => f.toOptionFragment _).getOrElse((_: String) => None))
      }
  }

  // modify to fragment

  object modifyToFragment extends Poly1 {
    implicit def atOptionModifier[K, V <: Option[Modifier[_]]](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
      at[FieldType[K, V]] { ft =>
        field[K](ft.map(_.toFragment))
      }
  }
}
