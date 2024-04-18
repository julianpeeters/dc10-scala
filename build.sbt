val CatsV = "2.10.0"
val Dc10V = "0.4.0+2-f918b98f+20240405-1512-SNAPSHOT"
val MUnitV = "0.7.29"
val SourcePosV = "1.1.0"

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
  scalaVersion := "3.4.1",
  versionScheme := Some("semver-spec"),
))

lazy val scala = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/scala"))
  .settings(
    name := "dc10-scala",
    libraryDependencies ++= Seq(
      // main
      "com.julianpeeters" %%% "dc10-core" % Dc10V,
      "org.tpolecat"      %%% "sourcepos" % SourcePosV,
      "org.typelevel"     %%% "cats-core" % CatsV,
      // test
      "org.scalameta"     %% "munit"     % MUnitV      % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val meta = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/dc10"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "dc10-scala-dc10",
    buildInfoKeys := Seq[BuildInfoKey](organization, name, version),
    buildInfoPackage := "dc10.scala.dc10",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta"     %% "munit"     % MUnitV      % Test
    )
  )
  .dependsOn(scala)
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val docs = project.in(file("docs/gitignored"))
  .settings(
    mdocOut := file("."),
    mdocVariables := Map(
      "SCALA" -> crossScalaVersions.value.map(e => e.takeWhile(_ != '.')).mkString(", "),
      "VERSION" -> version.value.takeWhile(_ != '+'),
    )
  )
  .dependsOn(scala.jvm)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)