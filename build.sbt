import sbtrelease._

val mainScala = "2.12.10"
val allScala  = Seq("2.13.1", mainScala)

name := "case-ql"

lazy val root = project
  .in(file("."))
  .settings(
    noPublishSettings,
    releaseSettings
  )
  .aggregate(
    `case-ql-sql`,
    `case-ql-circe`,
    `case-ql-gql`,
    `case-ql-example`
  )

lazy val `case-ql-sql` = project
  .in(file("modules/sql"))
  .settings(settings)
  .settings(
    name := "case-ql-sql",
    libraryDependencies ++= Dependencies.Jars.`case-ql-sql`,
    publishSettings,
    Defaults.itSettings
  )
  .configs(IntegrationTest)

lazy val `case-ql-circe` = project
  .in(file("modules/circe"))
  .dependsOn(
    `case-ql-sql` % "test->test;compile->compile"
  )
  .settings(settings)
  .settings(
    name := "case-ql-circe",
    libraryDependencies ++= Dependencies.Jars.`case-ql-circe`,
    publishSettings,
    Defaults.itSettings
  )
  .configs(IntegrationTest)

lazy val `case-ql-gql` = project
  .in(file("modules/gql"))
  .dependsOn(
    `case-ql-sql` % "test->test;compile->compile",
    `case-ql-circe` % "test->test;compile->compile"
  )
  .settings(settings)
  .settings(
    name := "case-ql-gql",
    libraryDependencies ++= Dependencies.Jars.`case-ql-gql`,
    publishSettings,
    Defaults.itSettings
  )
  .configs(IntegrationTest)

lazy val `case-ql-example` = project
  .in(file("modules/example"))
  .dependsOn(
    `case-ql-sql` % "test->compile;compile->compile",
    `case-ql-gql` % "test->compile;compile->compile",
    `case-ql-circe` % "test->compile;compile->compile"
  )
  .settings(settings)
  .settings(
    name := "case-ql-example",
    libraryDependencies ++= Dependencies.Jars.`case-ql-example`,
    noPublishSettings,
    javaOptions in Compile += "-Dlog4j.configurationFile=src/main/resources/log4j2.yml",
    javaOptions in Test += "-Dlog4j.configurationFile=src/test/resources/log4j2-test.yml",
    javaOptions in IntegrationTest += "-Dlog4j.configurationFile=src/it/resources/log4j2-it.yml"
  )

lazy val settings = commonSettings ++ scalafmtSettings ++ updateSettings

// common settings

lazy val commonSettings = scalacSettings ++ Seq(
  organization := "com.github.reddone",
  licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  scalaVersion := mainScala,
  crossScalaVersions := allScala,
  fork := true,
  parallelExecution in Test := false,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % Versions.kindProjectorVersion)
)

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",                                       // Emit warning and location for usages of deprecated APIs.
    "-encoding", /*format: off */ "utf-8" /*format: on*/, // Specify character encoding used by source files.
    "-explaintypes",                                      // Explain type errors in more detail.
    "-feature",                                           // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",                             // Existential types (besides wildcard types) can be written and inferred
    "-language:higherKinds",                              // Allow higher-kinded types
    "-language:implicitConversions",                      // Allow definition of implicit functions called views
    "-unchecked",                                         // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                                        // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                                   // Fail the compilation if there are any warnings.
    "-Xfuture",                                           // Turn on future language features.
    "-Xlint:by-name-right-associative",                   // By-name parameter of right associative operator.
    "-Xlint:constant",                                    // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",                          // Selecting member of DelayedInit.
    "-Xlint:doc-detached",                                // A Scaladoc comment appears to be detached from its element.
    "-Xlint:missing-interpolator",                        // A string literal appears to be missing an interpolator id.
    "-Xlint:option-implicit",                             // Option.apply used implicit view.
    "-Xlint:package-object-classes",                      // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",                      // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",                              // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                                 // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",                       // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",                               // Pattern match may not be typesafe.
    "-Ywarn-dead-code",                                   // Warn when dead code is identified.
    "-Ywarn-numeric-widen",                               // Warn when numerics are widened.
    //"-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    //"-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    //"-Ywarn-unused:locals", // Warn if a local definition is unused.
    //"-Ywarn-unused:params", // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    //"-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-Ywarn-macros:before", // via som
    "-Yrangepos"            // for longer squiggles.
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Seq(
        "-Xsource:2.13",
        "-Xlint:adapted-args",     // Warn if an argument list is modified to match the receiver.
        "-Xlint:infer-any",        // Warn when a type argument is inferred to be `Any`.
        "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Xlint:nullary-unit",     // Warn when nullary methods return Unit.
        "-Yno-adapted-args",       // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
        "-Ypartial-unification",   // Enable partial unification in type constructor inference
        "-Ywarn-extra-implicit",   // Warn when more than one implicit parameter section is defined.
        "-Ywarn-inaccessible",     // Warn about inaccessible types in method signatures.
        "-Ywarn-infer-any",        // Warn when a type argument is inferred to be `Any`.
        "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Ywarn-nullary-unit",     // Warn when nullary methods return Unit.
        "-opt-inline-from:<source>",
        "-opt-warnings",
        "-opt:l:inline"
      )
    case _ => Nil
  }),
  scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports")
)

// scalafmt settings

lazy val scalafmtSettings = Seq(
  scalafmtOnCompile := true
)

// update settings

lazy val updateSettings = Seq(
  updateOptions := updateOptions.value.withGigahorse(false),
  dependencyUpdatesFilter -= moduleFilter(organization = "org.scala-lang")
)

// release settings

lazy val releaseSettings = Seq(
  // strip the qualifier off the input version, eg. 1.2.1-SNAPSHOT -> 1.2.1
  releaseVersion := { ver =>
    Version(ver)
      .map(_.withoutQualifier.string)
      .getOrElse(versionFormatError(ver))
  },
  // bump the version and append '-SNAPSHOT', eg. 1.2.1 -> 1.3.0-SNAPSHOT
  releaseNextVersion := { ver =>
    Version(ver)
      .map(_.bump(releaseVersionBump.value).asSnapshot.string)
      .getOrElse(versionFormatError(ver))
  },
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

// publish settings

lazy val publishSettings = Seq(
  publishTo in ThisBuild := sonatypePublishToBundle.value
)

lazy val noPublishSettings = Seq(
  skip in publish := true
)
