package com.github.reddone.caseql.sql

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models.{
  IntFilter,
  LongFilter,
  StringFilter,
  StringFilterOption,
  TimestampFilterOption
}
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.models.{
  IntModifier,
  LongModifier,
  StringModifier,
  StringModifierOption,
  TimestampModifierOption
}
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier

object TestModel {

  // TABLE

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  // simple case, should compile
  case class TestKey(
      field1: Int,
      field3: Long
  )
  // simple case but unordered, should compile
  case class TestKeyUnordered(
      field3: Long,
      field1: Int
  )
  // with other field, should not compile
  case class TestKeyOther(
      field1: Int,
      field3: Long,
      field5: Option[String]
  )
  // with other field and unordered, should not compile
  case class TestKeyOtherUnordered(
      field5: Option[String],
      field3: Long,
      field1: Int
  )

  // FILTER

  // simple case, should compile
  case class TestFilter(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      AND: Option[Seq[TestFilter]],
      OR: Option[Seq[TestFilter]],
      NOT: Option[TestFilter]
  ) extends EntityFilter[TestFilter]
  object TestFilter {
    val empty: TestFilter = TestFilter(None, None, None, None, None, None, None)
  }
  // simple case but unordered, should compile
  case class TestFilterUnordered(
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field1: Option[IntFilter],
      AND: Option[Seq[TestFilterUnordered]],
      OR: Option[Seq[TestFilterUnordered]],
      NOT: Option[TestFilterUnordered]
  ) extends EntityFilter[TestFilterUnordered]
  object TestFilterUnordered {
    val empty: TestFilterUnordered = TestFilterUnordered(None, None, None, None, None, None, None)
  }
  // with other fields, should compile
  case class TestFilterOther(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      otherField1: String,
      otherField2: Seq[Int],
      AND: Option[Seq[TestFilterOther]],
      OR: Option[Seq[TestFilterOther]],
      NOT: Option[TestFilterOther]
  ) extends EntityFilter[TestFilterOther]
  object TestFilterOther {
    val empty: TestFilterOther = TestFilterOther(None, None, None, None, "", Seq.empty, None, None, None)
  }
  // with other fields and unordered, should compile
  case class TestFilterOtherUnordered(
      otherField2: Seq[Int],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      otherField1: String,
      field1: Option[IntFilter],
      AND: Option[Seq[TestFilterOtherUnordered]],
      OR: Option[Seq[TestFilterOtherUnordered]],
      NOT: Option[TestFilterOtherUnordered]
  ) extends EntityFilter[TestFilterOtherUnordered]
  object TestFilterOtherUnordered {
    val empty: TestFilterOtherUnordered =
      TestFilterOtherUnordered(Seq.empty, None, None, None, "", None, None, None, None)
  }
  // one more field, should not compile
  case class TestFilterPlus(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      field5: Option[StringFilter],
      AND: Option[Seq[TestFilterPlus]],
      OR: Option[Seq[TestFilterPlus]],
      NOT: Option[TestFilterPlus]
  ) extends EntityFilter[TestFilterPlus]
  object TestFilterPlus {
    val empty: TestFilterPlus = TestFilterPlus(None, None, None, None, None, None, None, None)
  }
  // one more field and unordered, should not compile
  case class TestFilterPlusUnordered(
      field1: Option[IntFilter],
      field5: Option[StringFilter],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterPlusUnordered]],
      OR: Option[Seq[TestFilterPlusUnordered]],
      NOT: Option[TestFilterPlusUnordered]
  ) extends EntityFilter[TestFilterPlusUnordered]
  object TestFilterPlusUnordered {
    val empty: TestFilterPlusUnordered = TestFilterPlusUnordered(None, None, None, None, None, None, None, None)
  }
  // one less field, should not compile
  case class TestFilterLess(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterLess]],
      OR: Option[Seq[TestFilterLess]],
      NOT: Option[TestFilterLess]
  ) extends EntityFilter[TestFilterLess]
  object TestFilterLess {
    val empty: TestFilterLess = TestFilterLess(None, None, None, None, None, None)
  }
  // one less field and unordered, should not compile
  case class TestFilterLessUnordered(
      field2: Option[StringFilterOption],
      field1: Option[IntFilter],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterLessUnordered]],
      OR: Option[Seq[TestFilterLessUnordered]],
      NOT: Option[TestFilterLessUnordered]
  ) extends EntityFilter[TestFilterLessUnordered]
  object TestFilterLessUnordered {
    val empty: TestFilterLessUnordered = TestFilterLessUnordered(None, None, None, None, None, None)
  }

  // MODIFIER

  // simple case, should compile
  case class TestModifier(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption]
  ) extends EntityModifier[TestModifier]
  object TestModifier {
    val empty: TestModifier = TestModifier(None, None, None, None)
  }
  // simple case but unordered, should compile
  case class TestModifierUnordered(
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field1: Option[IntModifier]
  ) extends EntityModifier[TestModifierUnordered]
  object TestModifierUnordered {
    val empty: TestModifierUnordered = TestModifierUnordered(None, None, None, None)
  }
  // with other fields, should compile
  case class TestModifierOther(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption],
      otherField1: String,
      otherField2: Seq[Int]
  ) extends EntityModifier[TestModifierOther]
  object TestModifierOther {
    val empty: TestModifierOther = TestModifierOther(None, None, None, None, "", Seq.empty)
  }
  // with other fields and unordered, should compile
  case class TestModifierOtherUnordered(
      otherField2: Seq[Int],
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      otherField1: String,
      field1: Option[IntModifier]
  ) extends EntityModifier[TestModifierOtherUnordered]
  object TestModifierOtherUnordered {
    val empty: TestModifierOtherUnordered = TestModifierOtherUnordered(Seq.empty, None, None, None, "", None)
  }
  // one more field, should not compile
  case class TestModifierPlus(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption],
      field5: Option[StringModifier]
  ) extends EntityModifier[TestModifierPlus]
  object TestModifierPlus {
    val empty: TestModifierPlus = TestModifierPlus(None, None, None, None, None)
  }
  // one more field and unordered, should not compile
  case class TestModifierPlusUnordered(
      field1: Option[IntModifier],
      field5: Option[StringModifier],
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierPlusUnordered]
  object TestModifierPlusUnordered {
    val empty: TestModifierPlusUnordered = TestModifierPlusUnordered(None, None, None, None, None)
  }
  // one less field, should not compile
  case class TestModifierLess(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierLess]
  object TestModifierLess {
    val empty: TestModifierLess = TestModifierLess(None, None, None)
  }
  // one less field and unordered, should not compile
  case class TestModifierLessUnordered(
      field2: Option[StringModifierOption],
      field1: Option[IntModifier],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierLessUnordered]
  object TestModifierLessUnordered {
    val empty: TestModifierLessUnordered = TestModifierLessUnordered(None, None, None)
  }
}
