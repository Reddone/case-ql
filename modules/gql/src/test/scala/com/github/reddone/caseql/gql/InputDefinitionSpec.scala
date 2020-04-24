package com.github.reddone.caseql.gql

import com.github.reddone.caseql.sql.filter.primitives._
import com.github.reddone.caseql.circe.filter.decoders._
import io.circe._
import io.circe.generic.semiauto._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sangria.macros.derive.{InputObjectTypeDescription, InputObjectTypeName, deriveInputObjectType}
import sangria.marshalling.circe._
import sangria.schema._
import sangria.validation.QueryValidator

class InputDefinitionSpec extends AnyFlatSpec with Matchers {

  case class InputFilter(
      aBoolean: Option[Filter[Boolean]],
      anOptionBoolean: Option[FilterOption[Boolean]]
  )

  object InputFilter {
    implicit val dec1: Decoder[Filter[Boolean]] =
      booleanFilterDecoder.map(_.asInstanceOf[Filter[Boolean]])
    implicit val dec2: Decoder[FilterOption[Boolean]] =
      booleanFilterOptionDecoder.map(_.asInstanceOf[FilterOption[Boolean]])
    implicit val decoder: Decoder[InputFilter] = deriveDecoder[InputFilter]
  }

  "InputDefinition macro" should "work with Filter and FilterOption" in {
    import InputDefinition._
    import sangria.ast.Document
    import sangria.parser.QueryParser
    import sangria.renderer.QueryRenderer
    import sangria.macros._

    implicit val inputFilterType: InputObjectType[InputFilter] = deriveInputObjectType[InputFilter](
      InputObjectTypeName("InputFilter"),
      InputObjectTypeDescription("InputFilter")
    )
    val inputFilterArg = Argument("input", OptionInputType(inputFilterType))

    val Query: ObjectType[Unit, Unit] = ObjectType[Unit, Unit](
      "Query",
      fields[Unit, Unit](
        Field(
          "test",
          StringType,
          Some("Test"),
          arguments = inputFilterArg :: Nil,
          resolve = _ => "Hey there!"
        )
      )
    )

    val schema: Schema[Unit, Unit] = Schema(query = Query, mutation = None, subscription = None, additionalTypes = Nil)

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
  }
}
