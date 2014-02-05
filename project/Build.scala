package joosbox

import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object JoosboxBuild extends Build {
  val sharedSettings = Project.defaultSettings ++ assemblySettings ++ Seq(
    organization := "joosbox",

    scalaVersion := "2.10.3",

    libraryDependencies := Seq(
      "org.scala-lang" % "scala-library" % "2.10.3",
      "org.specs2" %% "specs2" % "2.3.7" % "test",
      "commons-lang" % "commons-lang" % "2.6"
    ),

    resolvers := Seq("snapshots", "releases").map(Resolver.sonatypeRepo),

    parallelExecution in Test := false,

    scalacOptions := Seq("-feature", "-unchecked", "-deprecation"),

    test in assembly := {}
  )

  // lazy val joosbox = Project(
  //   id = "joosbox",
  //   base = file("."),
  //   settings = sharedSettings
  // ).settings(
  //   name := "joosbox",
  //   mainClass in assembly := Some("joosbox.compiler.CompilerRunner"),
  // ).aggregate(
  //   lexer,
  //   parser,
  //   compiler,
  //   core
  // )

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
    settings = sharedSettings
  ).settings(
    name := "joosbox-lexer"
  ).dependsOn(core)

  lazy val parser = Project(
    id = "joosbox-parser",
    base = file("joosbox-parser"),
    settings = sharedSettings
  ).settings(
    name := "joosbox-parser"
  ).dependsOn(core, lexer)

  lazy val compiler = Project(
    id = "joosbox-compiler",
    base = file("joosbox-compiler"),
    settings = sharedSettings ++ Seq(
      mainClass := Some("joosbox.compiler.CompilerRunner")
    )
  ).settings(
    name := "joosbox-compiler"
  ).dependsOn(core, lexer, parser)
}

