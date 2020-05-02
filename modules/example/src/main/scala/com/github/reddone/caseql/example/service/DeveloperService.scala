package com.github.reddone.caseql.example.service

import cats._
import cats.effect._
import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.example.model.implicits._
import com.github.reddone.caseql.sql.table.Table
import doobie._
import doobie.implicits._

trait DeveloperService[F[_]] {
  def getDevelopers(filter: DeveloperFilter): F[List[Developer]]
  def getDeveloperById(key: DeveloperKey): F[Option[Developer]]
  def insertDeveloper(modifier: DeveloperModifier): F[DeveloperKey]
  def updateDevelopers(modifier: DeveloperModifier, filter: DeveloperFilter): F[List[DeveloperKey]]
  def updateDeveloperById(modifier: DeveloperModifier, key: DeveloperKey): F[Option[DeveloperKey]]
  def deleteDevelopers(filter: DeveloperFilter): F[List[DeveloperKey]]
  def deleteDeveloperById(key: DeveloperKey): F[Option[DeveloperKey]]
}

object DeveloperService {

  def production[F[_]: Effect: Monad](xa: Transactor[F]): DeveloperService[F] = new DeveloperService[F] {
    override def getDevelopers(filter: DeveloperFilter): F[List[Developer]] =
      Table[Developer, DeveloperKey]
        .select(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def getDeveloperById(key: DeveloperKey): F[Option[Developer]] =
      Table[Developer, DeveloperKey]
        .selectByKey(key)
        .execute
        .transact(xa)

    override def insertDeveloper(modifier: DeveloperModifier): F[DeveloperKey] =
      Table[Developer, DeveloperKey]
        .insertReturningKey(modifier)
        .execute
        .transact(xa)

    override def updateDevelopers(modifier: DeveloperModifier, filter: DeveloperFilter): F[List[DeveloperKey]] =
      Table[Developer, DeveloperKey]
        .updateReturningKeys(modifier, filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def updateDeveloperById(modifier: DeveloperModifier, key: DeveloperKey): F[Option[DeveloperKey]] =
      Table[Developer, DeveloperKey]
        .updateByKeyReturningKeys(modifier, key)
        .execute
        .transact(xa)
        .compile
        .last

    override def deleteDevelopers(filter: DeveloperFilter): F[List[DeveloperKey]] =
      Table[Developer, DeveloperKey]
        .deleteReturningKeys(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def deleteDeveloperById(key: DeveloperKey): F[Option[DeveloperKey]] =
      Table[Developer, DeveloperKey]
        .deleteByKeyReturningKeys(key)
        .execute
        .transact(xa)
        .compile
        .last
  }
}
