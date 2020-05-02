package com.github.reddone.caseql.example.service

import cats.effect.Effect
import com.github.reddone.caseql.example.model.db.{Developer, DeveloperFilter, DeveloperKey, DeveloperModifier}
import doobie.util.transactor.Transactor

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

  def production[F[_]: Effect](xa: Transactor[F]): DeveloperService[F] = new DeveloperService[F] {
    override def getDevelopers(filter: DeveloperFilter): F[List[Developer]] = ???

    override def getDeveloperById(key: DeveloperKey): F[Option[Developer]] = ???

    override def insertDeveloper(modifier: DeveloperModifier): F[DeveloperKey] = ???

    override def updateDevelopers(modifier: DeveloperModifier, filter: DeveloperFilter): F[List[DeveloperKey]] = ???

    override def updateDeveloperById(modifier: DeveloperModifier, key: DeveloperKey): F[Option[DeveloperKey]] = ???

    override def deleteDevelopers(filter: DeveloperFilter): F[List[DeveloperKey]] = ???

    override def deleteDeveloperById(key: DeveloperKey): F[Option[DeveloperKey]] = ???
  }
}
