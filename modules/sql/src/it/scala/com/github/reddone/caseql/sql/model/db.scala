package com.github.reddone.caseql.sql.model

object db {

  final case class Employee(
    id: Long,
    name: String,
    homePlanet: String
                        )

}
