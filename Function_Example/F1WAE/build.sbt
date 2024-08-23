import Dependencies._

ThisBuild / scalaVersion     := "2.13.13"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Function_Example",
    libraryDependencies += munit % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
