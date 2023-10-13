lazy val V = _root_.scalafix.sbt.BuildInfo
inThisBuild(
  List(
    organization := "ch.epfl.scala",
    homepage := Some(url("https://github.com/kotleta2007/scala3-syntax-rewrite")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kotleta2007",
        "Mark Tropin",
        "mark.tropin@epfl.ch",
        url("https://epfl.ch")
      )
    ),
    scalaVersion := V.scala213,
    // TODO: remove these lines when changed to SyntacticRule
    semanticdbEnabled := true,
    semanticdbIncludeInJar := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++= List("-Yrangepos")
    // classLoaderLayeringStrategy in Compile := ClassLoaderLayeringStrategy.Flat
  )
)

(publish / skip) := true

lazy val rules = project.settings(
  moduleName := "named-literal-arguments",
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
)

lazy val input = project.settings(
  scalaVersion := "3.3.1",
  (publish / skip) := true
)

lazy val output = project.settings(
  scalaVersion := "3.3.1",
  (publish / skip) := true
)

lazy val tests = project
  .settings(
    // scalaVersion := "2.13.12",
    scalaVersion := "3.3.1",
    (publish / skip) := true,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    scalafixTestkitOutputSourceDirectories :=
      (output / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputSourceDirectories :=
      (input / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputClasspath :=
      (input / Compile / fullClasspath).value
  )
  .dependsOn(rules)
  .enablePlugins(ScalafixTestkitPlugin)