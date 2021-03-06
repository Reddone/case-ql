package com.github.reddone.caseql.sql.table

import java.sql.Timestamp
import java.time.Instant
import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.itmodel.data._
import com.github.reddone.caseql.sql.itmodel.db._
import com.github.reddone.caseql.sql.itmodel.implicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.modifier.primitives._
import com.github.reddone.caseql.sql.filter.primitives._
import com.github.reddone.caseql.sql.filter.wrappers.RelationFilter
import com.github.reddone.caseql.sql.util.TestTransactors._
import doobie.implicits._

class TableQueryItSpec extends PgAnyWordSpec {

  val currentDeveloperId = new AtomicLong(developers.length.toLong)
  val currentProjectId   = new AtomicLong(projects.length.toLong)
  val currentTaskId      = new AtomicLong(tasks.length.toLong)

  "TableQuery" when {

    "selecting data" should {

      "succeed to execute a simple select" in {
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val selectedDevelopers = Table[Developer, DeveloperKey]
          .select(filter, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L))
        )
      }

      "succeed to execute a select with a nested filter" in {
        val nestedFilter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(LT = Some(32))),
          OR = Some(
            Seq(
              DeveloperFilter.empty.copy(name = Some(StringFilter.empty.copy(CONTAINS = Some("ino")))),
              DeveloperFilter.empty.copy(id = Some(LongFilter.empty.copy(EQ = Some(3L))))
            )
          )
        )

        val selectedDevelopers = Table[Developer, DeveloperKey]
          .select(nestedFilter, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L)),
          Developer(5L, "Cyberino Matterino", 2, Some(4L))
        )
      }

      "succeed to execute a select with a deep nested filter" in {
        val deepNestedFilter = DeveloperFilter.empty.copy(
          OR = Some(
            Seq(
              DeveloperFilter.empty.copy(
                AND = Some(
                  Seq(
                    DeveloperFilter.empty.copy(
                      name = Some(
                        StringFilter.empty.copy(CONTAINS = Some("done"))
                      )
                    ),
                    DeveloperFilter.empty.copy(
                      NOT = Some(
                        DeveloperFilter.empty.copy(
                          teamLeaderId = Some(LongFilterOption.empty.copy(IS_NULL = Some(false)))
                        )
                      )
                    )
                  )
                )
              ),
              DeveloperFilter.empty.copy(
                AND = Some(
                  Seq(
                    DeveloperFilter.empty.copy(
                      age = Some(
                        IntFilter.empty.copy(EQ = Some(1))
                      )
                    ),
                    DeveloperFilter.empty.copy(
                      NOT = Some(
                        DeveloperFilter.empty.copy(
                          teamLeaderId = Some(LongFilterOption.empty.copy(IS_NULL = Some(true)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

        val selectedDevelopers = Table[Developer, DeveloperKey]
          .select(deepNestedFilter, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers should contain theSameElementsAs List(
          Developer(1L, "Reddone", 32, None),
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L))
        )
      }

      "succeed to execute a select with a self relation filter" in {
        val relationFilter1 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )
        val filter1 = DeveloperFilter.empty.copy(
          selfRelation = Some(
            RelationFilter
              .selfEmpty[Developer, DeveloperFilter]
              .copy(EVERY = Some(relationFilter1))
          )
        )

        val selectedDevelopers1 = Table[Developer, DeveloperKey]
          .select(filter1, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers1 should contain theSameElementsAs List(
          Developer(1L, "Reddone", 32, None)
        )

        val relationFilter2 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )
        val filter2 = DeveloperFilter.empty.copy(
          selfRelation = Some(
            RelationFilter
              .selfEmpty[Developer, DeveloperFilter]
              .copy(SOME = Some(relationFilter2))
          )
        )

        val selectedDevelopers2 = Table[Developer, DeveloperKey]
          .select(filter2, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers2 should contain theSameElementsAs List(
          Developer(1L, "Reddone", 32, None)
        )

        val relationFilter3 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )
        val filter3 = DeveloperFilter.empty.copy(
          selfRelation = Some(
            RelationFilter
              .selfEmpty[Developer, DeveloperFilter]
              .copy(NONE = Some(relationFilter3))
          )
        )

        val selectedDevelopers3 = Table[Developer, DeveloperKey]
          .select(filter3, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers3 should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L)),
          Developer(4L, "Maximus Kappacus Spamicus", 23, None),
          Developer(5L, "Cyberino Matterino", 2, Some(4L)),
          Developer(6L, "Mario Rigatone", 2, Some(4L))
        )
      }

      "succeed to execute a select with a direct relation filter" in {
        val relationFilter1 = TaskFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter1 = ProjectFilter.empty.copy(
          taskRelation = Some(
            RelationFilter
              .empty[Project, Task, TaskFilter]
              .copy(EVERY = Some(relationFilter1))
          )
        )

        val selectedProjects1 = Table[Project, ProjectKey]
          .select(filter1, Some("p"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedProjects1 should contain theSameElementsAs List(
          Project(1L, "Topdeckin N' Wreckin", Some("Kripp most loved hobby"))
        )

        val relationFilter2 = TaskFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter2 = ProjectFilter.empty.copy(
          taskRelation = Some(
            RelationFilter
              .empty[Project, Task, TaskFilter]
              .copy(SOME = Some(relationFilter2))
          )
        )

        val selectedProjects2 = Table[Project, ProjectKey]
          .select(filter2, Some("p"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedProjects2 should contain theSameElementsAs List(
          Project(1L, "Topdeckin N' Wreckin", Some("Kripp most loved hobby")),
          Project(3L, "Welcome to Summoner Rift", Some("Fedding like there's no tomorrow"))
        )

        val relationFilter3 = TaskFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter3 = ProjectFilter.empty.copy(
          taskRelation = Some(
            RelationFilter
              .empty[Project, Task, TaskFilter]
              .copy(NONE = Some(relationFilter3))
          )
        )

        val selectedProjects3 = Table[Project, ProjectKey]
          .select(filter3, Some("p"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedProjects3 should contain theSameElementsAs List(
          Project(2L, "Random Pasta", None)
        )
      }

      "succeed to execute a select with a junction relation filter" in {
        val relationFilter1 = ProjectFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter1 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(LTE = Some(2))),
          projectRelation = Some(
            RelationFilter
              .empty[Developer, Project, ProjectFilter]
              .copy(EVERY = Some(relationFilter1))
          )
        )

        val selectedDevelopers1 = Table[Developer, DeveloperKey]
          .select(filter1, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers1 should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L))
        )

        val relationFilter2 = ProjectFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter2 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(LTE = Some(2))),
          projectRelation = Some(
            RelationFilter
              .empty[Developer, Project, ProjectFilter]
              .copy(SOME = Some(relationFilter2))
          )
        )

        val selectedDevelopers2 = Table[Developer, DeveloperKey]
          .select(filter2, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers2 should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L)),
          Developer(5L, "Cyberino Matterino", 2, Some(4L))
        )

        val relationFilter3 = ProjectFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false)))
        )
        val filter3 = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(LTE = Some(2))),
          projectRelation = Some(
            RelationFilter
              .empty[Developer, Project, ProjectFilter]
              .copy(NONE = Some(relationFilter3))
          )
        )

        val selectedDevelopers3 = Table[Developer, DeveloperKey]
          .select(filter3, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers3 should contain theSameElementsAs List(
          Developer(6L, "Mario Rigatone", 2, Some(4L))
        )
      }

      "succeed to execute a select with a deep relation filter" in {
        val nestedRelationFilter = TaskFilter.empty.copy(
          deadline = Some(
            TimestampFilter.empty.copy(LTE = Some(Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 2))))
          )
        )
        val relationFilter = ProjectFilter.empty.copy(
          description = Some(StringFilterOption.empty.copy(IS_NULL = Some(false))),
          taskRelation = Some(
            RelationFilter
              .empty[Project, Task, TaskFilter]
              .copy(SOME = Some(nestedRelationFilter))
          )
        )
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(LTE = Some(2))),
          projectRelation = Some(
            RelationFilter
              .empty[Developer, Project, ProjectFilter]
              .copy(SOME = Some(relationFilter))
          )
        )

        val selectedDevelopers = Table[Developer, DeveloperKey]
          .select(filter, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        selectedDevelopers should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L)),
          Developer(5L, "Cyberino Matterino", 2, Some(4L))
        )
      }

      "succeed to execute a simple select by key" in {
        val key = DeveloperKey(1L)

        val maybeSelectedDeveloper = Table[Developer, DeveloperKey]
          .selectByKey(key, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeSelectedDeveloper shouldBe defined
        maybeSelectedDeveloper.get shouldBe Developer(1L, "Reddone", 32, None)
      }
    }

    "inserting data" should {

      "succeed to execute a simple insert" in {
        val nextTaskId = currentTaskId.incrementAndGet()

        val modifier = TaskModifier.empty.copy(
          id = Some(LongModifier(ModifierAction.Set, Some(nextTaskId))),
          label = Some(StringModifier(ModifierAction.Set, Some("label"))),
          description = Some(StringModifierOption(ModifierOptionAction.Set, Some("description"))),
          deadline = Some(
            TimestampModifier(
              ModifierAction.Set,
              Some(Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 4)))
            )
          ),
          projectId = Some(LongModifier(ModifierAction.Set, Some(3L)))
        )

        val affectedRows = Table[Task, TaskKey]
          .insert(modifier)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute an insert with null values" in {
        val nextTaskId = currentTaskId.incrementAndGet()

        val modifier = TaskModifier.empty.copy(
          id = Some(LongModifier(ModifierAction.Set, Some(nextTaskId))),
          label = Some(StringModifier(ModifierAction.Set, Some("label"))),
          description = Some(StringModifierOption(ModifierOptionAction.Null, None)),
          deadline = Some(
            TimestampModifier(
              ModifierAction.Set,
              Some(Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 4)))
            )
          ),
          projectId = Some(LongModifier(ModifierAction.Set, Some(3L)))
        )

        val affectedRows = Table[Task, TaskKey]
          .insert(modifier)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute an insert with missing values having a default" in {
        val nextTaskId = currentTaskId.incrementAndGet()

        val modifier = TaskModifier.empty.copy(
          id = Some(LongModifier(ModifierAction.Set, Some(nextTaskId))),
          label = Some(StringModifier(ModifierAction.Set, Some("label"))),
          deadline = Some(
            TimestampModifier(
              ModifierAction.Set,
              Some(Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 4)))
            )
          ),
          projectId = Some(LongModifier(ModifierAction.Set, Some(3L)))
        )

        val affectedRows = Table[Task, TaskKey]
          .insert(modifier)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute a simple insert returning key" in {
        val nextTaskId = currentTaskId.incrementAndGet()

        val modifier = TaskModifier.empty.copy(
          id = Some(LongModifier(ModifierAction.Set, Some(nextTaskId))),
          label = Some(StringModifier(ModifierAction.Set, Some("label"))),
          description = Some(StringModifierOption(ModifierOptionAction.Set, Some("description"))),
          deadline = Some(
            TimestampModifier(
              ModifierAction.Set,
              Some(Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 4)))
            )
          ),
          projectId = Some(LongModifier(ModifierAction.Set, Some(3L)))
        )

        val returnedKey = Table[Task, TaskKey]
          .insertReturningKey(modifier)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        returnedKey shouldBe TaskKey(nextTaskId)
      }
    }

    "updating data" should {

      "succeed to execute a simple update" in {
        val modifier = DeveloperModifier.empty.copy(
          age = Some(IntModifier(ModifierAction.Set, Some(3))),
          teamLeaderId = Some(LongModifierOption(ModifierOptionAction.Null, None))
        )

        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val affectedRows = Table[Developer, DeveloperKey]
          .update(modifier, filter)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 2
      }

      "succeed to execute a simple update returning keys" in {
        val modifier = DeveloperModifier.empty.copy(
          age = Some(IntModifier(ModifierAction.Set, Some(3))),
          teamLeaderId = Some(LongModifierOption(ModifierOptionAction.Null, None))
        )

        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val returnedKeys = Table[Developer, DeveloperKey]
          .updateReturningKeys(modifier, filter)
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(DeveloperKey(2L), DeveloperKey(3L))
      }

      "succeed to execute a simple update by key" in {
        val modifier = DeveloperModifier.empty.copy(
          age = Some(IntModifier(ModifierAction.Set, Some(42))),
          teamLeaderId = Some(LongModifierOption(ModifierOptionAction.Set, Some(1L)))
        )

        val key = DeveloperKey(4L)

        val affectedRows = Table[Developer, DeveloperKey]
          .updateByKey(modifier, key)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute a simple update by key returning keys" in {
        val modifier = DeveloperModifier.empty.copy(
          age = Some(IntModifier(ModifierAction.Set, Some(42))),
          teamLeaderId = Some(LongModifierOption(ModifierOptionAction.Set, Some(1L)))
        )

        val key = DeveloperKey(4L)

        val returnedKeys = Table[Developer, DeveloperKey]
          .updateByKeyReturningKeys(modifier, key)
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(DeveloperKey(4L))
      }
    }

    "deleting data" should {

      "succeed to execute a simple delete" in {
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val affectedRows = Table[Developer, DeveloperKey]
          .delete(filter)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 2
      }

      "succeed to execute a simple delete returning keys" in {
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val returnedKeys = Table[Developer, DeveloperKey]
          .deleteReturningKeys(filter)
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(DeveloperKey(2L), DeveloperKey(3L))
      }

      "succeed to execute a simple delete by key" in {
        val key = DeveloperKey(4L)

        val affectedRows = Table[Developer, DeveloperKey]
          .deleteByKey(key)
          .execute
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute a simple delete by key returning keys" in {
        val key = DeveloperKey(4L)

        val returnedKeys = Table[Developer, DeveloperKey]
          .deleteByKeyReturningKeys(key)
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(DeveloperKey(4L))
      }
    }
  }
}
