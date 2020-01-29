package com.github.reddone.caseql.sql.config

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DoobieConfigSpec extends AnyFlatSpec with Matchers {

  "DoobieConfig" should "succeed to read a valid configuration" in {
    val config1: Config = ConfigFactory.parseString(s"""|doobie {
                                                        |  numThreads = 20
                                                        |  driverClassName = "driver"
                                                        |  url = "url"
                                                        |  user = "user"
                                                        |  password = "password"
                                                        |}""".stripMargin)

    val result = DoobieConfig.valueOf(config1)

    result.numThreads shouldBe 20
    result.driverClassName shouldBe "driver"
    result.url shouldBe "url"
    result.user shouldBe "user"
    result.password shouldBe "password"
  }

  it should "fail to read an invalid configuration" in {
    val config1: Config = ConfigFactory.parseString(s"""|doobie {
                                                        |  numThreads = -1
                                                        |  driverClassName = "driver"
                                                        |  url = "url"
                                                        |  user = "user"
                                                        |  password = "password"
                                                        |}""".stripMargin)

    val expected1 = "numThreads cannot be negative"
    val thrown1   = the[IllegalArgumentException] thrownBy DoobieConfig.valueOf(config1)
    thrown1.getMessage shouldBe "requirement failed: " + expected1

    val config2: Config = ConfigFactory.parseString(s"""|doobie {
                                                        |  numThreads = 20
                                                        |  driverClassName = ""
                                                        |  url = "url"
                                                        |  user = "user"
                                                        |  password = "password"
                                                        |}""".stripMargin)

    val expected2 = "driverClassName cannot be empty"
    val thrown2   = the[IllegalArgumentException] thrownBy DoobieConfig.valueOf(config2)
    thrown2.getMessage shouldBe "requirement failed: " + expected2

    val config3: Config = ConfigFactory.parseString(s"""|doobie {
                                                        |  numThreads = 20
                                                        |  driverClassName = "driver"
                                                        |  url = ""
                                                        |  user = "user"
                                                        |  password = "password"
                                                        |}""".stripMargin)

    val expected3 = "url cannot be empty"
    val thrown3   = the[IllegalArgumentException] thrownBy DoobieConfig.valueOf(config3)
    thrown3.getMessage shouldBe "requirement failed: " + expected3

    val config4: Config = ConfigFactory.parseString(s"""|doobie {
                                                        |  numThreads = 20
                                                        |  driverClassName = "driver"
                                                        |  url = "url"
                                                        |  user = ""
                                                        |  password = "password"
                                                        |}""".stripMargin)

    val expected4 = "user cannot be empty"
    val thrown4   = the[IllegalArgumentException] thrownBy DoobieConfig.valueOf(config4)
    thrown4.getMessage shouldBe "requirement failed: " + expected4
  }
}
