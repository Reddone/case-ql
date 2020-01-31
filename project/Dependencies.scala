import Versions._
import sbt._

object Dependencies {

  object typesafe {
    lazy val namespace = "com.typesafe"
    lazy val config    = namespace % "config" % configVersion
  }

  object shapeless {
    lazy val namespace = "com.chuusai"
    lazy val core      = namespace %% "shapeless" % shapelessVersion
  }

  object circe {
    lazy val namespace = "io.circe"
    lazy val core      = namespace %% "circe-core" % circeVersion
    lazy val parser    = namespace %% "circe-parser" % circeVersion
    lazy val generic   = namespace %% "circe-generic" % circeVersion
    lazy val optics    = namespace %% "circe-optics" % circeOpticsVersion
  }

  object cats {
    lazy val namespace = "org.typelevel"
    lazy val core      = namespace %% "cats-core" % catsVersion
    lazy val free      = namespace %% "cats-free" % catsVersion
    lazy val effect    = namespace %% "cats-effect" % catsEffectVersion
  }

  object fs2 {
    lazy val namespace = "co.fs2"
    lazy val core      = namespace %% "fs2-core" % fs2Version
  }

  object doobie {
    lazy val namespace = "org.tpolecat"
    lazy val core      = namespace %% "doobie-core" % doobieVersion
    lazy val hikari    = namespace %% "doobie-hikari" % doobieVersion
    lazy val postgres  = namespace %% "doobie-postgres" % doobieVersion
    lazy val scalatest = namespace %% "doobie-scalatest" % doobieVersion
  }

  object sangria {
    lazy val namespace = "org.sangria-graphql"
    lazy val core      = namespace %% "sangria" % sangriaVersion
    lazy val circe     = namespace %% "sangria-circe" % sangriaCirceVersion
  }

  object scalatest {
    lazy val namespace = "org.scalatest"
    lazy val core      = namespace %% "scalatest" % scalatestVersion
  }

  object testcontainers {
    lazy val namespace = "org.testcontainers"
    lazy val postgres  = namespace % "postgresql" % testcontainersPostgresVersion
  }

  object testcontainersScala {
    lazy val namespace = "com.dimafeng"
    lazy val core      = namespace %% "testcontainers-scala" % testcontainersScalaVersion
  }

  object slf4j {
    lazy val namespace = "org.slf4j"
    lazy val api       = namespace % "slf4j-api" % slf4jVersion
  }

  object akka {
    lazy val namespace = "com.typesafe.akka"
    lazy val slf4j     = namespace %% "akka-slf4j" % akkaVersion
    lazy val actor     = namespace %% "akka-actor" % akkaVersion
    lazy val stream    = namespace %% "akka-stream" % akkaVersion
    lazy val http      = namespace %% "akka-http" % akkaHttpVersion
  }

  object akkaHttpCirce {
    lazy val namespace = "de.heikoseeberger"
    lazy val core      = namespace %% "akka-http-circe" % akkaHttpCirceVersion
  }

  object log4j {
    lazy val namespace = "org.apache.logging.log4j"
    lazy val core      = namespace % "log4j-core" % log4jVersion
    lazy val api       = namespace % "log4j-api" % log4jVersion
    lazy val impl      = namespace % "log4j-slf4j-impl" % log4jVersion
    lazy val scala     = namespace %% "log4j-api-scala" % log4jScalaVersion
  }

  object jacksonCore {
    lazy val namespace = "com.fasterxml.jackson.core"
    lazy val batabind  = namespace % "jackson-databind" % jacksonVersion
  }

  object jacksonDataFormat {
    lazy val namespace = "com.fasterxml.jackson.dataformat"
    lazy val yaml      = namespace % "jackson-dataformat-yaml" % jacksonVersion
  }

  object Jars {
    lazy val `shared`: Seq[ModuleID] = Seq(
      //slf4j.api                % "compile",
      scalatest.core           % "it, test",
      testcontainers.postgres  % "it",
      testcontainersScala.core % "it"
    )

    lazy val `sql`: Seq[ModuleID] = Seq(
      typesafe.config  % "compile",
      shapeless.core   % "compile",
      circe.core       % "compile",
      circe.parser     % "compile",
      circe.generic    % "compile",
      cats.core        % "compile",
      cats.free        % "compile",
      cats.effect      % "compile",
      fs2.core         % "compile",
      doobie.core      % "compile",
      doobie.scalatest % "it",
      doobie.postgres  % "it"
    ) ++ `shared`

    lazy val `gql`: Seq[ModuleID] = Seq(
      sangria.core  % "compile",
      sangria.circe % "compile"
    ) ++ `shared`

    lazy val `example`: Seq[ModuleID] = Seq(
      typesafe.config        % "compile",
      slf4j.api              % "compile",
      log4j.core             % "compile",
      log4j.api              % "compile",
      log4j.impl             % "compile",
      log4j.scala            % "compile",
      jacksonCore.batabind   % "compile",
      jacksonDataFormat.yaml % "compile",
      circe.core             % "compile",
      circe.parser           % "compile",
      circe.generic          % "compile",
      circe.optics           % "compile",
      doobie.core            % "compile",
      doobie.hikari          % "compile",
      doobie.postgres        % "compile",
      akka.slf4j             % "compile",
      akka.actor             % "compile",
      akka.stream            % "compile",
      akka.http              % "compile",
      akkaHttpCirce.core     % "compile",
      sangria.core           % "compile",
      sangria.circe          % "compile"
    )
  }
}
