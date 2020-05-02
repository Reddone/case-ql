package com.github.reddone.caseql.example.service

import cats.effect.Effect
import com.github.reddone.caseql.example.model.db.{Task, TaskFilter, TaskKey, TaskModifier}
import doobie.util.transactor.Transactor

trait TaskService[F[_]] {
  def getTasks(filter: TaskFilter): F[List[Task]]
  def getTaskById(key: TaskKey): F[Option[Task]]
  def insertTask(modifier: TaskModifier): F[TaskKey]
  def updateTasks(modifier: TaskModifier, filter: TaskFilter): F[List[TaskKey]]
  def updateTaskById(modifier: TaskModifier, key: TaskKey): F[Option[TaskKey]]
  def deleteTasks(filter: TaskFilter): F[List[TaskKey]]
  def deleteTaskById(key: TaskKey): F[Option[TaskKey]]
}

object TaskService {

  def production[F[_]: Effect](xa: Transactor[F]): TaskService[F] = new TaskService[F] {
    override def getTasks(filter: TaskFilter): F[List[Task]] = ???

    override def getTaskById(key: TaskKey): F[Option[Task]] = ???

    override def insertTask(modifier: TaskModifier): F[TaskKey] = ???

    override def updateTasks(modifier: TaskModifier, filter: TaskFilter): F[List[TaskKey]] = ???

    override def updateTaskById(modifier: TaskModifier, key: TaskKey): F[Option[TaskKey]] = ???

    override def deleteTasks(filter: TaskFilter): F[List[TaskKey]] = ???

    override def deleteTaskById(key: TaskKey): F[Option[TaskKey]] = ???
  }
}
