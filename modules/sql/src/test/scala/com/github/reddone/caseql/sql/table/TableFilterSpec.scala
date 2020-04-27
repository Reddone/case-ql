package com.github.reddone.caseql.sql.table

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.model.db._
import com.github.reddone.caseql.sql.filter.primitives._
import com.github.reddone.caseql.sql.filter.wrappers.RelationFilter
import com.github.reddone.caseql.sql.table.TableLink.Aux
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableFilterSpec extends AnyFlatSpec with Matchers {

  implicit val table: Table[Test, TestKey] =
    Table.derive[Test, TestKey](useTableAlias = false)
  implicit val leftTable: Table[TestLeft, TestLeftKey] =
    Table.derive[TestLeft, TestLeftKey](useTableAlias = false)
  implicit val directTable: Table[TestDirect, TestDirectKey] =
    Table.derive[TestDirect, TestDirectKey](useTableAlias = false)
  implicit val rightTable: Table[TestRight, TestRightKey] =
    Table.derive[TestRight, TestRightKey](useTableAlias = false)
  implicit val junctionTable: Table[TestJunction, TestJunctionKey] =
    Table.derive[TestJunction, TestJunctionKey](useTableAlias = false)

  implicit val leftSelfLink: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
    FieldSet("field1"),
    FieldSet("field3")
  )
  implicit val directLeftLink: Aux[TestDirect, TestLeft, Unit] = TableLink.direct[TestDirect, TestLeft](
    FieldSet("field3"),
    FieldSet("field1")
  )
  implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] = TableLink.direct[TestLeft, TestJunction](
    FieldSet("field1"),
    FieldSet("field1")
  )
  implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] = TableLink.direct[TestRight, TestJunction](
    FieldSet("field1"),
    FieldSet("field2")
  )
  implicit val leftRightLink: Aux[TestLeft, TestRight, TestJunction] =
    TableLink.union(leftJunctionLink, rightJunctionLink)

  "TableFilter derivation" should "compile in the simple case" in {
    """TableFilter.derive[Test, TestFilter]()""" should compile
  }

  it should "compile in the unordered case" in {
    """TableFilter.derive[Test, TestFilterUnordered]()""" should compile
  }

  it should "not compile in the other case" in {
    """TableFilter.derive[Test, TestFilterOther]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterOther]()""" }
  }

  it should "not compile in the other unordered case" in {
    """TableFilter.derive[Test, TestFilterOtherUnordered]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterOtherUnordered]()""" }
  }

  it should "not compile in the plus case" in {
    """TableFilter.derive[Test, TestFilterPlus]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterPlus]()""" }
  }

  it should "not compile in the plus unordered case" in {
    """TableFilter.derive[Test, TestFilterPlusUnordered]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterPlusUnordered]()""" }
  }

  it should "compile in the less case" in {
    """TableFilter.derive[Test, TestFilterLess]()""" should compile
  }

  it should "compile in the less unordered case" in {
    """TableFilter.derive[Test, TestFilterLessUnordered]()""" should compile
  }

  "TableFilter typeclass" should "work correctly with EntityFilter" in {
    val tableFilter1: TableFilter[Test, TestFilter] =
      TableFilter.derive[Test, TestFilter]()
    val filter1 = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1), IN = Some(Seq(2, 3)))),
      field2 = Some(StringFilterOption.empty.copy(CONTAINS = Some("A")))
    )
    val alias1  = "a1"
    val result1 = tableFilter1.primitiveFilterFragments(filter1)

    result1(Some(alias1)).map(_.map(_.toString)) shouldBe List(
      Some("Fragment(\"(a1.field1 = ? ) AND (a1.field1 IN (?, ?) ) \")"),
      Some("Fragment(\"(a1.field2 LIKE ? ) \")"),
      None,
      None
    )

    val tableFilter2: TableFilter[Test, TestFilterUnordered] =
      TableFilter.derive[Test, TestFilterUnordered]()
    val filter2 = TestFilterUnordered.empty.copy(
      field2 = Some(StringFilterOption.empty.copy(CONTAINS = Some("A"))),
      field1 = Some(IntFilter.empty.copy(EQ = Some(1), IN = Some(Seq(2, 3))))
    )
    val alias2  = "a2"
    val result2 = tableFilter2.primitiveFilterFragments(filter2)

    result2(Some(alias2)).map(_.map(_.toString)) shouldBe List(
      None,
      Some("Fragment(\"(a2.field2 LIKE ? ) \")"),
      None,
      Some("Fragment(\"(a2.field1 = ? ) AND (a2.field1 IN (?, ?) ) \")")
    )
  }

  "TableFilter combinator" should "work correctly with an empty EntityFilter" in {
    val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()

    val filter1 = TestFilter.empty
    val alias1  = "a1"
    val result1 = tableFilter.byFilterFragment(filter1, Some(alias1))

    result1 shouldBe None

    val filter2 = TestFilter(
      Some(IntFilter.empty),
      Some(StringFilterOption.empty),
      Some(LongFilter.empty),
      Some(TimestampFilterOption.empty),
      None,
      None,
      None
    )
    val alias2  = "a2"
    val result2 = tableFilter.byFilterFragment(filter2, Some(alias2))

    result2 shouldBe None
  }

  it should "work correctly with a flat EntityFilter" in {
    val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()

    val filter = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1), IN = Some(Seq(1, 1, 1)))),
      Some(StringFilterOption.empty.copy(EQ = Some("2"), CONTAINS = Some("2"))),
      Some(LongFilter.empty.copy(EQ = Some(3L), IN = Some(Seq(3L, 3L, 3L)))),
      Some(TimestampFilterOption.empty.copy(EQ = Some(Timestamp.from(Instant.EPOCH)))),
      None,
      None,
      None
    )
    val alias  = "a1"
    val result = tableFilter.byFilterFragment(filter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "(" +
      "((a1.field1 = ? ) AND (a1.field1 IN (?, ?, ?) ) ) AND " +
      "((a1.field2 = ? ) AND (a1.field2 LIKE ? ) ) AND " +
      "((a1.field3 = ? ) AND (a1.field3 IN (?, ?, ?) ) ) AND " +
      "((a1.field4 = ? ) ) " +
      ") " +
      "\")"
  }

  it should "work correctly with a nested EntityFilter" in {
    val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()

    val filter = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(11))),
      Some(StringFilterOption.empty.copy(EQ = Some("22"))),
      None,
      None,
      None,
      None,
      None
    )
    val nestedFilter = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      Some(StringFilterOption.empty.copy(EQ = Some("2"))),
      None,
      None,
      AND = Some(Seq(filter, filter)),
      OR = Some(Seq(filter, filter)),
      NOT = Some(filter)
    )
    val alias  = "a1"
    val result = tableFilter.byFilterFragment(nestedFilter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      "AND " +
      "((" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      ") AND (" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      ") ) " +
      "AND " +
      "((" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      ") OR (" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      ") ) " +
      "AND " +
      "(NOT (" +
      "(" +
      "((a1.field1 = ? ) ) AND ((a1.field2 = ? ) ) " +
      ") " +
      ") ) " +
      "\")"
  }

  it should "work correctly with a deeply nested EntityFilter" in {
    val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()

    // danger: a LISP enthusiast may want to test a deeper filter
    val deepFilter = TestFilter.empty.copy(OR = Some(
      Seq(
        TestFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(11)))),
        TestFilter.empty.copy(field2 = Some(StringFilterOption.empty.copy(EQ = Some("22")))),
        TestFilter.empty.copy(AND = Some(
          Seq(
            TestFilter.empty.copy(
              field3 = Some(LongFilter.empty.copy(EQ = Some(333L), IN = Some(Seq(333L, 333L, 333L)))),
              field4 = Some(TimestampFilterOption.empty.copy(EQ = Some(Timestamp.from(Instant.EPOCH))))
            ),
            TestFilter.empty.copy(NOT =
              Some(TestFilter.empty.copy(field2 = Some(StringFilterOption.empty.copy(CONTAINS = Some("2222")))))
            ),
            TestFilter.empty.copy(NOT = Some(
              TestFilter.empty.copy(field1 = Some(IntFilter.empty.copy(IN = Some(Seq(1111, 1111)))))
            )
            )
          )
        )
        )
      )
    )
    )
    val alias  = "a1"
    val result = tableFilter.byFilterFragment(deepFilter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "((" + // OR BEGIN
      "(" +
      "((a1.field1 = ? ) ) " + // FIRST FILTER INSIDE OR
      ") " +
      ") OR (" +
      "(" +
      "((a1.field2 = ? ) ) " + // SECOND FILTER INSIDE OR
      ") " +
      ") OR (" +
      "((" + // THIRD FILTER INSIDE OR, AND BEGIN
      "(" +
      "((a1.field3 = ? ) AND (a1.field3 IN (?, ?, ?) ) ) AND ((a1.field4 = ? ) ) " + // FIRST FILTER INSIDE AND
      ") " +
      ") AND (" +
      "(NOT (" + // SECOND FILTER INSIDE AND, NOT BEGIN
      "(" +
      "((a1.field2 LIKE ? ) ) " +
      ") " +
      ") ) " + // NOT END
      ") AND (" +
      "(NOT (" + // THIRD FILTER INSIDE AND, NOT BEGIN
      "(" +
      "((a1.field1 IN (?, ?) ) ) " +
      ") " +
      ") ) " + // NOT END
      ") ) " + // AND END
      ") ) " + // OR END
      "\")"
  }

  "TableFilter relation" should "work correctly with a self RelationFilter" in {
    implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()
    implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()

    val leftFilter = TestLeftFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1))),
      selfRelation = Some(
        RelationFilter
          .selfEmpty[TestLeft, TestLeftFilter]
          .copy(
            EVERY = Some(TestLeftFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("EVERY"))))),
            SOME = Some(TestLeftFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("SOME"))))),
            NONE = Some(TestLeftFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("NONE")))))
          )
      )
    )
    val alias1  = "a1"
    val result1 = leftTableFilter.byFilterFragment(leftFilter, Some(alias1))

    result1 shouldBe defined
    result1.get.toString shouldBe "Fragment(\"" +
      "(((a1.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN SELF RELATION
      "(" + // BEGIN EVERY
      "(SELECT COUNT (*) FROM test_left WHERE a1.field1 = test_left.field3 AND (((test_left.field2 = ? ) ) ) ) " +
      "= " +
      "(SELECT COUNT (*) FROM test_left WHERE a1.field1 = test_left.field3) " +
      ") " + // END EVERY
      "AND " +
      "(" + // BEGIN SOME
      "EXISTS (SELECT 1 FROM test_left WHERE a1.field1 = test_left.field3 AND (((test_left.field2 = ? ) ) ) ) " +
      ") " + // END SOME
      "AND " +
      "(" + // BEGIN NONE
      "NOT EXISTS (SELECT 1 FROM test_left WHERE a1.field1 = test_left.field3 AND (((test_left.field2 = ? ) ) ) ) " +
      ") " + // END NONE
      ") " + // END SELF RELATION
      ") " +
      "\")"

    val result2 = leftTableFilter.byFilterFragment(leftFilter, None)

    result2 shouldBe defined
    result2.get.toString shouldBe "Fragment(\"" +
      "(((test_left.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN SELF RELATION
      "(" + // BEGIN EVERY
      "(SELECT COUNT (*) FROM test_left self WHERE test_left.field1 = self.field3 AND (((self.field2 = ? ) ) ) ) " +
      "= " +
      "(SELECT COUNT (*) FROM test_left self WHERE test_left.field1 = self.field3) " +
      ") " + // END EVERY
      "AND " +
      "(" + // BEGIN SOME
      "EXISTS (SELECT 1 FROM test_left self WHERE test_left.field1 = self.field3 AND (((self.field2 = ? ) ) ) ) " +
      ") " + // END SOME
      "AND " +
      "(" + // BEGIN NONE
      "NOT EXISTS (SELECT 1 FROM test_left self WHERE test_left.field1 = self.field3 AND (((self.field2 = ? ) ) ) ) " +
      ") " + // END NONE
      ") " + // END SELF RELATION
      ") " +
      "\")"
  }

  it should "work correctly with a direct RelationFilter" in {
    implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()
    implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()
    implicit val directTableFilter: TableFilter[TestDirect, TestDirectFilter] =
      TableFilter.derive[TestDirect, TestDirectFilter]()

    val directFilter = TestDirectFilter.empty.copy(
      field1 = Some(StringFilter.empty.copy(EQ = Some("1"))),
      leftRelation = Some(
        RelationFilter
          .empty[TestDirect, TestLeft, TestLeftFilter]
          .copy(
            EVERY = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1))))),
            SOME = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1))))),
            NONE = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1)))))
          )
      )
    )
    val alias  = "a1"
    val result = directTableFilter.byFilterFragment(directFilter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "(((a1.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN DIRECT RELATION
      "(" + // BEGIN EVERY
      "(SELECT COUNT (*) FROM test_left WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) ) " +
      "= " +
      "(SELECT COUNT (*) FROM test_left WHERE a1.field3 = test_left.field1) " +
      ") " + // END EVERY
      "AND " +
      "(" + // BEGIN SOME
      "EXISTS (SELECT 1 FROM test_left WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) ) " +
      ") " + // END SOME
      "AND " +
      "(" + // BEGIN NONE
      "NOT EXISTS (SELECT 1 FROM test_left WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) ) " +
      ") " + // END NONE
      ") " + // END DIRECT RELATION
      ") " +
      "\")"
  }

  it should "work correctly with a junction RelationFilter" in {
    implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()
    implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()

    val rightFilter = TestRightFilter.empty.copy(
      field1 = Some(LongFilter.empty.copy(EQ = Some(1L))),
      leftRelation = Some(
        RelationFilter
          .empty[TestRight, TestLeft, TestLeftFilter]
          .copy(
            EVERY = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1))))),
            SOME = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1))))),
            NONE = Some(TestLeftFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1)))))
          )
      )
    )

    val alias  = "a1"
    val result = rightTableFilter.byFilterFragment(rightFilter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "(((a1.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN JUNCTION RELATION
      "(" + // BEGIN EVERY
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "LEFT OUTER JOIN test_left ON test_left.field1 = test_junction.field1 " +
      "WHERE a1.field1 = test_junction.field2 AND IS NULL test_left.field1 AND (((test_left.field1 = ? ) ) ) ) " +
      ") " + // END EVERY
      "AND " +
      "(" + // BEGIN SOME
      "EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_left ON test_left.field1 = test_junction.field1 " +
      "WHERE a1.field1 = test_junction.field2 AND (((test_left.field1 = ? ) ) ) ) " +
      ") " + // END SOME
      "AND " +
      "(" + // BEGIN NONE
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_left ON test_left.field1 = test_junction.field1 " +
      "WHERE a1.field1 = test_junction.field2 AND (((test_left.field1 = ? ) ) ) ) " +
      ") " + // END NONE
      ") " + // END JUNCTION RELATION
      ") " +
      "\")"
  }

  it should "work correctly with a nested RelationFilter" in {
    implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()
    implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()
    implicit val directTableFilter: TableFilter[TestDirect, TestDirectFilter] =
      TableFilter.derive[TestDirect, TestDirectFilter]()

    val directNestedFilter = TestDirectFilter.empty.copy(
      field1 = Some(StringFilter.empty.copy(EQ = Some("1"))),
      leftRelation = Some(
        RelationFilter
          .empty[TestDirect, TestLeft, TestLeftFilter]
          .copy(
            EVERY = Some(
              TestLeftFilter.empty.copy(
                field1 = Some(IntFilter.empty.copy(EQ = Some(1))),
                rightRelation = Some(
                  RelationFilter
                    .empty[TestLeft, TestRight, TestRightFilter]
                    .copy(
                      EVERY = Some(TestRightFilter.empty.copy(field1 = Some(LongFilter.empty.copy(EQ = Some(1L))))),
                      SOME = Some(TestRightFilter.empty.copy(field1 = Some(LongFilter.empty.copy(EQ = Some(1L))))),
                      NONE = Some(TestRightFilter.empty.copy(field1 = Some(LongFilter.empty.copy(EQ = Some(1L)))))
                    )
                )
              )
            ),
            SOME = Some(
              TestLeftFilter.empty.copy(
                field1 = Some(IntFilter.empty.copy(EQ = Some(2))),
                rightRelation = Some(
                  RelationFilter
                    .empty[TestLeft, TestRight, TestRightFilter]
                    .copy(
                      EVERY = Some(TestRightFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("2"))))),
                      SOME = Some(TestRightFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("2"))))),
                      NONE = Some(TestRightFilter.empty.copy(field2 = Some(StringFilter.empty.copy(EQ = Some("2")))))
                    )
                )
              )
            ),
            NONE = Some(
              TestLeftFilter.empty.copy(
                field1 = Some(IntFilter.empty.copy(EQ = Some(3))),
                rightRelation = Some(
                  RelationFilter
                    .empty[TestLeft, TestRight, TestRightFilter]
                    .copy(
                      EVERY = Some(TestRightFilter.empty.copy(field3 = Some(IntFilter.empty.copy(EQ = Some(1))))),
                      SOME = Some(TestRightFilter.empty.copy(field3 = Some(IntFilter.empty.copy(EQ = Some(1))))),
                      NONE = Some(TestRightFilter.empty.copy(field3 = Some(IntFilter.empty.copy(EQ = Some(1)))))
                    )
                )
              )
            )
          )
      )
    )
    val alias  = "a1"
    val result = directTableFilter.byFilterFragment(directNestedFilter, Some(alias))

    result shouldBe defined
    result.get.toString shouldBe "Fragment(\"" +
      "(((a1.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN DIRECT RELATION
      "(" + // BEGIN EVERY
      "(SELECT COUNT (*) FROM test_left WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN JUNCTION RELATION INSIDE DIRECT RELATION
      "(" + // BEGIN NESTED EVERY
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "LEFT OUTER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 " +
      "AND IS NULL test_right.field1 AND (((test_right.field1 = ? ) ) ) ) " +
      ") " + // END NESTED EVERY
      "AND " +
      "(" + // BEGIN NESTED SOME
      "EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field1 = ? ) ) ) ) " +
      ") " + // END NESTED SOME
      "AND " +
      "(" + // BEGIN NESTED NONE
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field1 = ? ) ) ) ) " +
      ") " + // END NESTED NONE
      ") " + // END JUNCTION RELATION INSIDE DIRECT RELATION
      ") " +
      ") " +
      "= " +
      "(SELECT COUNT (*) FROM test_left WHERE a1.field3 = test_left.field1) " +
      ") " + // END EVERY
      "AND " +
      "(" + // BEGIN SOME
      "EXISTS (" +
      "SELECT 1 FROM test_left " +
      "WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN JUNCTION RELATION INSIDE DIRECT RELATION
      "(" + // BEGIN NESTED EVERY
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "LEFT OUTER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 " +
      "AND IS NULL test_right.field1 AND (((test_right.field2 = ? ) ) ) ) " +
      ") " + // END NESTED EVERY
      "AND " +
      "(" + // BEGIN NESTED SOME
      "EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field2 = ? ) ) ) ) " +
      ") " + // END NESTED SOME
      "AND " +
      "(" + // BEGIN NESTED NONE
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field2 = ? ) ) ) ) " +
      ") " + // END NESTED NONE
      ") " + // END JUNCTION RELATION INSIDE NESTED RELATION
      ") " +
      ") " +
      ") " + // END SOME
      "AND " +
      "(" + // BEGIN NONE
      "NOT EXISTS (" +
      "SELECT 1 FROM test_left " +
      "WHERE a1.field3 = test_left.field1 AND (((test_left.field1 = ? ) ) ) " +
      "AND " +
      "(" +
      "(" + // BEGIN JUNCTION RELATION INSIDE DIRECT RELATION
      "(" + // BEGIN NESTED EVERY
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "LEFT OUTER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 " +
      "AND IS NULL test_right.field1 AND (((test_right.field3 = ? ) ) ) ) " +
      ") " + // END NESTED EVERY
      "AND " +
      "(" + // BEGIN NESTED SOME
      "EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field3 = ? ) ) ) ) " +
      ") " + // END NESTED SOME
      "AND " +
      "(" + // BEGIN NESTED NONE
      "NOT EXISTS (" +
      "SELECT 1 FROM test_junction " +
      "INNER JOIN test_right ON test_right.field1 = test_junction.field2 " +
      "WHERE test_left.field1 = test_junction.field1 AND (((test_right.field3 = ? ) ) ) ) " +
      ") " + // END NESTED NONE
      ") " + // END JUNCTION RELATION INSIDE DIRECT RELATION
      ") " +
      ") " +
      ") " + // END NONE
      ") " + // END DIRECT RELATION
      ") " +
      "\")"
  }
}
