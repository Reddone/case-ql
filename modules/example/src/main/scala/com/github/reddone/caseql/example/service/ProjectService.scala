package com.github.reddone.caseql.example.service

import cats._
import cats.effect._
import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.example.model.implicits._
import com.github.reddone.caseql.sql.table.Table
import doobie._
import doobie.implicits._

trait ProjectService[F[_]] {
  def getProjects(filter: ProjectFilter): F[List[Project]]
  def getProjectById(key: ProjectKey): F[Option[Project]]
  def insertProject(modifier: ProjectModifier): F[ProjectKey]
  def updateProjects(modifier: ProjectModifier, filter: ProjectFilter): F[List[ProjectKey]]
  def updateProjectById(modifier: ProjectModifier, key: ProjectKey): F[Option[ProjectKey]]
  def deleteProjects(filter: ProjectFilter): F[List[ProjectKey]]
  def deleteProjectById(key: ProjectKey): F[Option[ProjectKey]]
}

object ProjectService {

  def production[F[_]: Effect: Monad](xa: Transactor[F]): ProjectService[F] = new ProjectService[F] {
    override def getProjects(filter: ProjectFilter): F[List[Project]] =
      Table[Project, ProjectKey]
        .select(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def getProjectById(key: ProjectKey): F[Option[Project]] =
      Table[Project, ProjectKey]
        .selectByKey(key)
        .execute
        .transact(xa)

    override def insertProject(modifier: ProjectModifier): F[ProjectKey] =
      Table[Project, ProjectKey]
        .insertReturningKey(modifier)
        .execute
        .transact(xa)

    override def updateProjects(modifier: ProjectModifier, filter: ProjectFilter): F[List[ProjectKey]] =
      Table[Project, ProjectKey]
        .updateReturningKeys(modifier, filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def updateProjectById(modifier: ProjectModifier, key: ProjectKey): F[Option[ProjectKey]] =
      Table[Project, ProjectKey]
        .updateByKeyReturningKeys(modifier, key)
        .execute
        .transact(xa)
        .compile
        .last

    override def deleteProjects(filter: ProjectFilter): F[List[ProjectKey]] =
      Table[Project, ProjectKey]
        .deleteReturningKeys(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def deleteProjectById(key: ProjectKey): F[Option[ProjectKey]] =
      Table[Project, ProjectKey]
        .deleteByKeyReturningKeys(key)
        .execute
        .transact(xa)
        .compile
        .last
  }
}
