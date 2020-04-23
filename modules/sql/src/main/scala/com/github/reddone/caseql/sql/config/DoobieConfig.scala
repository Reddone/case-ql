package com.github.reddone.caseql.sql.config

abstract case class DoobieConfig(
    numThreads: Int,
    driverClassName: String,
    url: String,
    user: String,
    password: String
)
