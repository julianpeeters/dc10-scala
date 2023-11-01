val CatsV = "2.10.0"
val Dc10V = "0.3.0"
val MUnitV = "0.7.29"
val SourcePosV = "1.1.0"
val TwiddlesV = "0.7.0"

inThisBuild(List(
  crossScalaVersions := Seq(scalaVersion.value),
  description := "A definitional compiler for generating Scala code.",
  organization := "com.julianpeeters",
  homepage := Some(url("https://github.com/julianpeeters/dc10-scala")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "julianpeeters",
      "Julian Peeters",
      "julianpeeters@gmail.com",
      url("http://github.com/julianpeeters")
    )
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Werror",
    "-source:future",
    "-Wunused:all",
    "-Wvalue-discard"
  ),
  scalaVersion := "3.4.0-RC1-bin-20231025-8046a8b-NIGHTLY",
  versionScheme := Some("semver-spec"),
))

lazy val scala = (project in file("."))
  .settings(
    name := "dc10-scala",
    libraryDependencies ++= Seq(
      // main
      "com.julianpeeters" %% "dc10-core" % Dc10V,
      "org.tpolecat"      %% "sourcepos" % SourcePosV,
      "org.typelevel"     %% "cats-free" % CatsV,
      // test
      "org.scalameta"     %% "munit"     % MUnitV      % Test
    )
  )

lazy val scalaq = (project in file("scalaq"))
  .settings(
    name := "dc10-scalaq",
    libraryDependencies ++= Seq(
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  ).dependsOn(scala)

lazy val docs = project.in(file("docs/gitignored"))
  .settings(
    mdocOut := scala.base,
    mdocVariables := Map(
      "SCALA" -> crossScalaVersions.value.map(e => e.takeWhile(_ != '.')).mkString(", "),
      "VERSION" -> version.value.takeWhile(_ != '+'),
    )
  )
  .dependsOn(scalaq)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)