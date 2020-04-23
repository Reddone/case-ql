package com.github.reddone.caseql.example.config

import com.github.reddone.caseql.sql.config.DoobieConfig
import com.typesafe.config.Config

object DoobieTypesafeConfig {

  def valueOf(config: Config): DoobieConfig = {
    val doobieConfig    = config.getConfig("doobie")
    val numThreads      = doobieConfig.getInt("numThreads")
    val driverClassName = doobieConfig.getString("driverClassName")
    val url             = doobieConfig.getString("url")
    val user            = doobieConfig.getString("user")
    val password        = doobieConfig.getString("password")
    require(numThreads >= 0, "numThreads cannot be negative")
    require(!driverClassName.isEmpty, "driverClassName cannot be empty")
    require(!url.isEmpty, "url cannot be empty")
    require(!user.isEmpty, "user cannot be empty")
    new DoobieConfig(numThreads, driverClassName, url, user, password) {}
  }
}
