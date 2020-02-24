package com.github.reddone.caseql.sql.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._
import shapeless.test.illTyped

class TableFunctionSpec extends AnyFlatSpec with Matchers {

  trait Wrapper[T]
  case class TestGeneric(
      field1: String,
      field2: Wrapper[String],
      field3: Option[Int],
      field4: Wrapper[Option[Int]]
  )

  "TableFunction extract" should "extract a generic type" in {
    object wrapperExtractor extends TableFunction.extract[Wrapper[_]]

    val generic = TestGeneric(
      "1",
      new Wrapper[String] {},
      Some(3),
      new Wrapper[Option[Int]] {}
    )

    type Correct = Wrapper[_] :: Wrapper[_] :: HNil
    type Wrong   = Wrapper[_] :: String :: HNil
    val result = LabelledGeneric[TestGeneric].to(generic).flatMap(wrapperExtractor)

    """implicitly[<:<[result.type, Correct]]""" should compile
    """implicitly[<:<[result.type, Wrong]]""" shouldNot compile
    illTyped { """implicitly[<:<[result.type, Wrong]]""" }
  }
}
