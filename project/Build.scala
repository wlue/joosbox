package joosbox

import sbt._
import Keys._

object JoosboxBuild extends Build {
  val sharedSettings = Project.defaultSettings ++ Seq(
    organization := "joosbox",

    scalaVersion := "2.10.3",

    libraryDependencies := Seq(
      "org.scala-lang" % "scala-library" % "2.10.3",
      "org.specs2" %% "specs2" % "2.3.7" % "test",
      "commons-lang" % "commons-lang" % "2.6"
    ),

    resolvers := Seq("snapshots", "releases").map(Resolver.sonatypeRepo),

    parallelExecution in Test := false,

    scalacOptions := Seq("-feature", "-unchecked")
  )

  lazy val joosbox = Project(
    id = "joosbox",
    base = file("."),
    settings = sharedSettings
  ).aggregate(
    lexer,
    compiler
  )

  lazy val core = Project(
    id = "joosbox-core",
    base = file("joosbox-core"),
    settings = sharedSettings
  ).settings(
    name := "joosbox-core"
  )

  lazy val lexer = Project(
    id = "joosbox-lexer",
    base = file("joosbox-lexer"),
    settings = sharedSettings ++ Seq(
      mainClass := Some("joosbox.lexer.LexerRunner")
    )
  ).settings(
    name := "joosbox-lexer"
  ).dependsOn(core)

  lazy val compiler = Project(
    id = "joosbox-compiler",
    base = file("joosbox-compiler"),
    settings = sharedSettings ++ Seq(
      mainClass := Some("joosbox.compiler.CompilerRunner")
    )
  ).settings(
    name := "joosbox-compiler"
  ).dependsOn(core)
}

