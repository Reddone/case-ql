package com.github.reddone.caseql.sql.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FromMapSpec extends AnyFlatSpec with Matchers {
  import ConvertHelper._

  case class Inner(field1: Option[String], field2: Int)
  case class Outer(field1: Int, field2: Option[String], field3: Inner)
  case class OuterWithOption(field1: Int, field2: Option[String], field3: Option[Inner])
  case class OuterWithList(field1: Int, field2: String, field3: List[Inner])

  "FromMap derivation" should "compile in every case" in {
    """CaseClassFromMap[Inner](Map.empty[String, Any])""" should compile
    """CaseClassFromMap[Outer](Map.empty[String, Any])""" should compile
    """CaseClassFromMap[OuterWithOption](Map.empty[String, Any])""" should compile
    """CaseClassFromMap[OuterWithList](Map.empty[String, Any])""" should compile
  }

  "FromMap converter" should "work in the simple case" in {
    val innerMap: Map[String, Any] = Map("field1" -> Some("1"), "field2" -> 2)
    val inner                      = Inner(Some("1"), 2)

    val result = to[Inner].from(innerMap)

    result shouldBe defined
    result.get shouldBe inner
  }

  it should "work in the outer case" in {
    val innerMap: Map[String, Any] = Map("field1" -> Some("1"), "field2" -> 2)
    val outerMap: Map[String, Any] = Map("field1" -> 1, "field2" -> Some("2"), "field3" -> innerMap)
    val outer                      = Outer(1, Some("2"), Inner(Some("1"), 2))

    val result = to[Outer].from(outerMap)

    result shouldBe defined
    result.get shouldBe outer
  }

  it should "work in the outer case with option" in {
    val innerMap: Map[String, Any]         = Map("field1" -> Some("1"), "field2" -> 2)
    val outerWithSomeMap: Map[String, Any] = Map("field1" -> 1, "field2" -> Some("2"), "field3" -> Some(innerMap))
    val outerWithNoneMap: Map[String, Any] = Map("field1" -> 1, "field2" -> Some("2"), "field3" -> None)
    val outerWithSome                      = OuterWithOption(1, Some("2"), Some(Inner(Some("1"), 2)))
    val outerWithNone                      = OuterWithOption(1, Some("2"), None)

    val result1 = to[OuterWithOption].from(outerWithSomeMap)
    val result2 = to[OuterWithOption].from(outerWithNoneMap)

    result1 shouldBe defined
    result1.get shouldBe outerWithSome
    result2 shouldBe defined
    result2.get shouldBe outerWithNone
  }

  it should "not work in the outer case with list" in {
    val innerMap: Map[String, Any]        = Map("field1" -> Some("1"), "field2" -> 2)
    val outerWithNelMap: Map[String, Any] = Map("field1" -> 1, "field2"         -> Some("2"), "field3" -> List(innerMap))
    val outerWithNilMap: Map[String, Any] = Map("field1" -> 1, "field2"         -> Some("2"), "field3" -> Nil)

    to[OuterWithList].from(outerWithNelMap) shouldBe None
    to[OuterWithList].from(outerWithNilMap) shouldBe None
  }
}
