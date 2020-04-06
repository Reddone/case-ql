package com.github.reddone.caseql.sql.table

import shapeless._
import shapeless.HList

trait EqualNoOption[L, R]

object EqualNoOption extends LowPriorityEqualNoOption {

  implicit def hcons[HL, HR, TL <: HList, TR <: HList](
      implicit
      equalHeads: EqualNoOption[HL, HR],
      equalTails: EqualNoOption[TL, TR]
  ): EqualNoOption[HL :: TL, HR :: TR] = new EqualNoOption[HL :: TL, HR :: TR] {}

  implicit def equalLeftIsOption[A, B](implicit equal: EqualNoOption[A, B]): EqualNoOption[Option[A], B] =
    new EqualNoOption[Option[A], B] {}

  implicit def equalRightIsOption[A, B](implicit equal: EqualNoOption[A, B]): EqualNoOption[A, Option[B]] =
    new EqualNoOption[A, Option[B]] {}
}

trait LowPriorityEqualNoOption {

  implicit def equal[A, B](implicit sameType: A =:= B): EqualNoOption[A, B] = new EqualNoOption[A, B] {}
}
