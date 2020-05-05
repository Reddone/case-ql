package com.github.reddone.caseql.example.service

import cats._
import cats.effect._
import com.github.reddone.caseql.example.model.db._
import com.github.reddone.caseql.example.model.implicits._
import com.github.reddone.caseql.sql.table.Table
import doobie._
import doobie.implicits._

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

  def production[F[_]: Effect: Monad](xa: Transactor[F]): TaskService[F] = new TaskService[F] {
    override def getTasks(filter: TaskFilter): F[List[Task]] =
      Table[Task, TaskKey]
        .select(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def getTaskById(key: TaskKey): F[Option[Task]] =
      Table[Task, TaskKey]
        .selectByKey(key)
        .execute
        .transact(xa)

    override def insertTask(modifier: TaskModifier): F[TaskKey] =
      Table[Task, TaskKey]
        .insertReturningKey(modifier)
        .execute
        .transact(xa)

    override def updateTasks(modifier: TaskModifier, filter: TaskFilter): F[List[TaskKey]] =
      Table[Task, TaskKey]
        .updateReturningKeys(modifier, filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def updateTaskById(modifier: TaskModifier, key: TaskKey): F[Option[TaskKey]] =
      Table[Task, TaskKey]
        .updateByKeyReturningKeys(modifier, key)
        .execute
        .transact(xa)
        .compile
        .last

    override def deleteTasks(filter: TaskFilter): F[List[TaskKey]] =
      Table[Task, TaskKey]
        .deleteReturningKeys(filter)
        .execute
        .transact(xa)
        .compile
        .toList

    override def deleteTaskById(key: TaskKey): F[Option[TaskKey]] =
      Table[Task, TaskKey]
        .deleteByKeyReturningKeys(key)
        .execute
        .transact(xa)
        .compile
        .last
  }
}
