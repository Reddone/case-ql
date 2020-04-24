package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.ByteTypeDefinition._
import com.github.reddone.caseql.gql.EnumDefinition._
import com.github.reddone.caseql.gql.JavaSqlTypeDefinition._
import com.github.reddone.caseql.gql.JavaTimeTypeDefinition._
import com.github.reddone.caseql.sql.model.db._
import sangria.macros.derive._
import sangria.schema._

object TestObjectDefinition {

  // TEST

  implicit val TestIdType: ObjectType[Unit, TestKey] =
    deriveObjectType[Unit, TestKey](
      ObjectTypeName("TestId"),
      ObjectTypeDescription("Test entity id")
    )

  implicit val TestType: ObjectType[Unit, Test] =
    deriveObjectType[Unit, Test](
      ObjectTypeName("Test"),
      ObjectTypeDescription("Test entity")
    )

  // TEST_LEFT

  implicit val TestLeftIdType: ObjectType[Unit, TestLeftKey] =
    deriveObjectType[Unit, TestLeftKey](
      ObjectTypeName("TestLeftId"),
      ObjectTypeDescription("TestLeft entity id")
    )

  implicit val TestLeftType: ObjectType[Unit, TestLeft] =
    deriveObjectType[Unit, TestLeft](
      ObjectTypeName("TestLeft"),
      ObjectTypeDescription("TestLeft entity")
    )

  // TEST_DIRECT

  implicit val TestDirectIdType: ObjectType[Unit, TestDirectKey] =
    deriveObjectType[Unit, TestDirectKey](
      ObjectTypeName("TestDirectId"),
      ObjectTypeDescription("TestDirect entity id")
    )

  implicit val TestDirectType: ObjectType[Unit, TestDirect] =
    deriveObjectType[Unit, TestDirect](
      ObjectTypeName("TestDirect"),
      ObjectTypeDescription("TestDirect entity")
    )

  // TEST_RIGHT

  implicit val TestRightIdType: ObjectType[Unit, TestRightKey] =
    deriveObjectType[Unit, TestRightKey](
      ObjectTypeName("TestRightId"),
      ObjectTypeDescription("TestRight entity id")
    )

  implicit val TestRightType: ObjectType[Unit, TestRight] =
    deriveObjectType[Unit, TestRight](
      ObjectTypeName("TestRight"),
      ObjectTypeDescription("TestRight entity")
    )

  // TEST_JUNCTION

  implicit val TestJunctionIdType: ObjectType[Unit, TestJunctionKey] =
    deriveObjectType[Unit, TestJunctionKey](
      ObjectTypeName("TestJunctionId"),
      ObjectTypeDescription("TestJunction entity id")
    )

  implicit val TestJunctionType: ObjectType[Unit, TestJunction] =
    deriveObjectType[Unit, TestJunction](
      ObjectTypeName("TestJunction"),
      ObjectTypeDescription("TestJunction entity")
    )
}
