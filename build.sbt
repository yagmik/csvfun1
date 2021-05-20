import Dependencies._

ThisBuild / scalaVersion     := "2.13.5"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "local.myagodin"
ThisBuild / organizationName := "Mikhail Yagodin"
ThisBuild / fork             := true
ThisBuild / scalacOptions    := Seq("-Xlint")

lazy val root = (project in file("."))
  .settings(
    name := "csvfun1",
    libraryDependencies ++= Seq(
      scalaTestFunSuite % Test,
      scalaParserCombinators
    ),
    scalacOptions ++= Seq("-Werror")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
