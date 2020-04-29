package com.github.reddone.caseql.gql

import com.github.reddone.caseql.sql.filter.primitives._
import com.github.reddone.caseql.circe.filter.decoders._
import com.github.reddone.caseql.gql.InputDefinition._
import sangria.ast.Document
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import io.circe._
import io.circe.generic.semiauto._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sangria.macros.derive.{InputObjectTypeDescription, InputObjectTypeName, deriveInputObjectType}
import sangria.marshalling.circe._
import sangria.macros._
import sangria.schema._
import sangria.validation.QueryValidator

class InputDefinitionSpec extends AnyFlatSpec with Matchers {

  case class InputFilter(
      aBoolean: Option[BooleanFilter],
      anOptionBoolean: Option[BooleanFilterOption]
  )

  implicit val inputFilterType: InputObjectType[InputFilter] = deriveInputObjectType[InputFilter](
    InputObjectTypeName("InputFilter"),
    InputObjectTypeDescription("InputFilter")
  )

  val Query: ObjectType[Unit, Unit] = ObjectType[Unit, Unit](
    "Query",
    fields[Unit, Unit](
      Field(
        "test",
        StringType,
        Some("Test"),
        arguments = Nil,
        resolve = _ => "Hey there!"
      )
    )
  )

  val schema: Schema[Unit, Unit] = Schema(
    query = Query,
    mutation = None,
    subscription = None,
    additionalTypes = inputFilterType :: Nil
  )

//  final case class AllFilter(
//      boolean: Option[Filter[Boolean]]
//  )
//
//  object AllFilter {
//    implicit val decoder: Decoder[AllFilter] = deriveDecoder[AllFilter]
//  }

  "InputDefinition macro" should "work with Filter and FilterOption" in {
    println(schema.toAst.renderPretty)

    val inp = gqlInpDoc"""
    {
      foo: "bar"
      bar: "foo"
      list: [
        {baz: RED}
        {baz: FOO_BAR}
        {test: 1}
        {}
      ]
    }
  """
    QueryValidator.default.validateInputDocument(schema, inp, "InputFilter")

    val inp2 = gqlInpDoc"""
    {
      aBoolean: {
        EQ: 3
        CC : 2
      }
      anOptionBoolean: {
        EQ: aaa
        CC: 4444
      }
    }
  """
    QueryValidator.default.validateInputDocument(schema, inp2, inputFilterType).foreach(println)
  }
}
