import sbtrelease._

inThisBuild(
  Seq(
    organization := "com.github.reddone",
    scmInfo := Some(
      ScmInfo(url("https://github.com/reddone/case-ql"), "git@github.com:reddone/case-ql.git")
    ),
    developers := List(
      Developer("Reddone", "Simone Marzoli", "simone88.rm2@gmail.com", url("https://github.com/reddone"))
    ),
    description := "Type-Safe and Serializable SQL Queries using Scala Case Classes",
    licenses ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
    homepage := Some(url("https://github.com/reddone/case-ql"))
  )
)

addCommandAlias("dist", "clean compile universal:packageBin")

lazy val root = project
  .in(file("."))
  .settings(
    name := "case-ql",
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
    `case-ql-sql` % "it->it;test->test;compile->compile"
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
    `case-ql-sql`   % "it->it;test->test;compile->compile",
    `case-ql-circe` % "it->it;test->test;compile->compile"
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
  .enablePlugins(JavaAppPackaging)
  .dependsOn(
    `case-ql-sql`,
    `case-ql-circe`,
    `case-ql-gql`
  )
  .settings(settings)
  .settings(
    name := "case-ql-example",
    libraryDependencies ++= Dependencies.Jars.`case-ql-example`,
    noPublishSettings,
    mainClass in Compile := Some("com.github.reddone.caseql.example.MainApp"),
    javaOptions in Compile += "-Dlog4j.configurationFile=src/main/resources/log4j2.yml"
  )

lazy val settings = commonSettings ++ scalacSettings ++ scalafmtSettings ++ updateSettings

// common settings

lazy val commonSettings = Seq(
  scalaVersion := "2.12.11",
  crossScalaVersions := Seq("2.13.2", "2.12.11"),
  fork := true,
  parallelExecution in Test := false,
  addCompilerPlugin(
    "org.typelevel" %% "kind-projector" % Versions.kindProjectorVersion cross CrossVersion.full
  )
)

// compiler settings

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",                                       // Emit warning and location for usages of deprecated APIs
    "-encoding", /*format: off */ "utf-8" /*format: on*/, // Specify character encoding used by source files
    "-explaintypes",                                      // Explain type errors in more detail
    "-feature",                                           // Emit warning and location for usages of features that should be imported explicitly
    "-language:existentials",                             // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",                      // Allow macro definition (besides implementation and application)
    "-language:higherKinds",                              // Allow higher-kinded types
    "-language:implicitConversions",                      // Allow definition of implicit functions called views
    "-unchecked",                                         // Enable additional warnings where generated code depends on assumptions
    "-Xcheckinit",                                        // Wrap field accessors to throw an exception on uninitialized access
    "-Xfatal-warnings",                                   // Fail the compilation if there are any warnings
    "-Xlint:adapted-args",                                // Warn if an argument list is modified to match the receiver
    "-Xlint:constant",                                    // Evaluation of a constant arithmetic expression results in an error
    "-Xlint:delayedinit-select",                          // Selecting member of DelayedInit
    "-Xlint:doc-detached",                                // A Scaladoc comment appears to be detached from its element
    "-Xlint:inaccessible",                                // Warn about inaccessible types in method signatures
    "-Xlint:infer-any",                                   // Warn when a type argument is inferred to be `Any`
    "-Xlint:missing-interpolator",                        // A string literal appears to be missing an interpolator id
    "-Xlint:nullary-override",                            // Warn when non-nullary `def f()' overrides nullary `def f'
    "-Xlint:nullary-unit",                                // Warn when nullary methods return Unit
    "-Xlint:option-implicit",                             // Option.apply used implicit view
    "-Xlint:package-object-classes",                      // Class or object defined in package object
    "-Xlint:poly-implicit-overload",                      // Parameterized overloaded implicit methods are not visible as view bounds
    "-Xlint:private-shadow",                              // A private field (or class parameter) shadows a superclass field
    "-Xlint:stars-align",                                 // Pattern sequence wildcard must align with sequence component
    "-Xlint:type-parameter-shadow",                       // A local type parameter shadows a type already in scope
    "-Ywarn-dead-code",                                   // Warn when dead code is identified
    "-Ywarn-extra-implicit",                              // Warn when more than one implicit parameter section is defined
    "-Ywarn-numeric-widen",                               // Warn when numerics are widened
    "-Ywarn-value-discard",                               // Warn when non-Unit expression results are unused
    "-Ywarn-macros:before",                               // Enable lint warnings on macro expansions
    "-Ybackend-parallelism",                              // Maximum worker threads for backend
    java.lang.Runtime.getRuntime.availableProcessors().toString
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Seq(
        "-Xsource:2.13",                    // Treat compiler input as Scala source for the specified version
        "-Xlint:by-name-right-associative", // By-name parameter of right associative operator
        "-Xlint:unsound-match",             // Pattern match may not be typesafe
        "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver
        "-Ypartial-unification"             // Enable partial unification in type constructor inference
      )
    case _ => Nil
  }),
  scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports")
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

// publish settings

lazy val publishSettings = Seq(
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  publishMavenStyle := true,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")
)

lazy val noPublishSettings = Seq(
  skip in publish := true
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
