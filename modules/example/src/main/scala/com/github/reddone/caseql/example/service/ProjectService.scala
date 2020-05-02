package com.github.reddone.caseql.example.service

import cats.effect.Effect
import com.github.reddone.caseql.example.model.db.{Project, ProjectFilter, ProjectKey, ProjectModifier}
import doobie.util.transactor.Transactor

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

  def production[F[_]: Effect](xa: Transactor[F]): ProjectService[F] = new ProjectService[F] {
    override def getProjects(filter: ProjectFilter): F[List[Project]] = ???

    override def getProjectById(key: ProjectKey): F[Option[Project]] = ???

    override def insertProject(modifier: ProjectModifier): F[ProjectKey] = ???

    override def updateProjects(modifier: ProjectModifier, filter: ProjectFilter): F[List[ProjectKey]] = ???

    override def updateProjectById(modifier: ProjectModifier, key: ProjectKey): F[Option[ProjectKey]] = ???

    override def deleteProjects(filter: ProjectFilter): F[List[ProjectKey]] = ???

    override def deleteProjectById(key: ProjectKey): F[Option[ProjectKey]] = ???
  }
}
