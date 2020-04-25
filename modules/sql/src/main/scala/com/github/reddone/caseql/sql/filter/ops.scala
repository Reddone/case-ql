package com.github.reddone.caseql.sql.filter

import cats.Show
import cats.data.NonEmptyList
import com.github.reddone.caseql.sql.tokens._
import doobie._
import doobie.implicits._
import Fragment._

object ops {

  def eq[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $Equal") ++ fr"$value")

  def notEq[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $NotEqual") ++ fr"$value")

  def lt[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $LessThan") ++ fr"$value")

  def lte[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $LessThanEqual") ++ fr"$value")

  def gt[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $GreaterThan") ++ fr"$value")

  def gte[T: Put](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $GreaterThanEqual") ++ fr"$value")

  def in[T: Put](name: String, valuesOpt: Option[Seq[T]]): Option[Fragment] =
    valuesOpt.map(values =>
      NonEmptyList.fromList(values.toList).map(Fragments.in(const(name), _)).getOrElse(const(False))
    )

  def notIn[T: Put](name: String, valuesOpt: Option[Seq[T]]): Option[Fragment] =
    valuesOpt.map(values =>
      NonEmptyList.fromList(values.toList).map(Fragments.notIn(const(name), _)).getOrElse(const(True))
    )

  def contains[T: Show](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $Like") ++ fr0"${"%" + Show[T].show(value) + "%"}")

  def notContains[T: Show](name: String, valueOpt: Option[T]): Option[Fragment] =
    valueOpt.map(value => const(s"$name $NotLike") ++ fr0"${"%" + Show[T].show(value) + "%"}")

  def containsEvery[T: Show](name: String, valuesOpt: Option[Seq[T]]): Option[Fragment] =
    valuesOpt.map(values => {
      val mapped = values.map(value => const(s"$name $Like") ++ fr0"${"%" + Show[T].show(value) + "%"}")
      Fragments.and(mapped: _*)
    })

  def containsSome[T: Show](name: String, valuesOpt: Option[Seq[T]]): Option[Fragment] =
    valuesOpt.map(values => {
      val mapped = values.map(value => const(s"$name $Like") ++ fr0"${"%" + Show[T].show(value) + "%"}")
      Fragments.or(mapped: _*)
    })

  def containsNone[T: Show](name: String, valuesOpt: Option[Seq[T]]): Option[Fragment] =
    valuesOpt.map(values => {
      val mapped = values.map(value => const(s"$name $NotLike") ++ fr0"${"%" + Show[T].show(value) + "%"}")
      Fragments.and(mapped: _*)
    })

  def isNull(name: String, valueOpt: Option[Boolean]): Option[Fragment] =
    valueOpt collect {
      case true  => const(s"$name $IsNull")
      case false => const(s"$name $IsNotNull")
    }
}
