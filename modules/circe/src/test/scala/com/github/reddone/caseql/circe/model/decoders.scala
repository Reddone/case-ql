package com.github.reddone.caseql.circe.model

import com.github.reddone.caseql.circe.filter.decoders._
import com.github.reddone.caseql.circe.modifier.decoders._
import com.github.reddone.caseql.circe.util.decoders._
import com.github.reddone.caseql.sql.TestModel._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object decoders {

  // TEST

  implicit val testDecoder: Decoder[Test] =
    deriveDecoder[Test]

  implicit val testKeyDecoder: Decoder[TestKey] =
    deriveDecoder[TestKey]

  implicit val testFilterDecoder: Decoder[TestFilter] =
    deriveDecoder[TestFilter]

  implicit val testModifierDecoder: Decoder[TestModifier] =
    deriveDecoder[TestModifier]

  // TEST_LEFT

  implicit val testLeftDecoder: Decoder[TestLeft] =
    deriveDecoder[TestLeft]

  implicit val testLeftKeyDecoder: Decoder[TestLeftKey] =
    deriveDecoder[TestLeftKey]

  implicit val testLeftFilterDecoder: Decoder[TestLeftFilter] =
    deriveDecoder[TestLeftFilter]

  // TEST_DIRECT

  implicit val testDirectDecoder: Decoder[TestDirect] =
    deriveDecoder[TestDirect]

  implicit val testDirectKeyDecoder: Decoder[TestDirectKey] =
    deriveDecoder[TestDirectKey]

  implicit val testDirectFilterDecoder: Decoder[TestDirectFilter] =
    deriveDecoder[TestDirectFilter]

  // TEST_RIGHT

  implicit val testRightDecoder: Decoder[TestRight] =
    deriveDecoder[TestRight]

  implicit val testRightKeyDecoder: Decoder[TestRightKey] =
    deriveDecoder[TestRightKey]

  implicit val testRightFilterDecoder: Decoder[TestRightFilter] =
    deriveDecoder[TestRightFilter]

  // TEST_JUNCTION

  implicit val testJunctionDecoder: Decoder[TestJunction] =
    deriveDecoder[TestJunction]

  implicit val testJunctionKeyDecoder: Decoder[TestJunctionKey] =
    deriveDecoder[TestJunctionKey]

  implicit val testJunctionFilterDecoder: Decoder[TestJunctionFilter] =
    deriveDecoder[TestJunctionFilter]
}
