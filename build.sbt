val Dc10V = "0.5.1"
val MUnitV = "1.0.2"

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
    "-Wunused:all",
    "-Xkind-projector:underscores"
  ),
  scalaVersion := "3.5.2",
  versionScheme := Some("semver-spec"),
))

lazy val scala = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/scala"))
  .settings(
    name := "dc10-scala",
    libraryDependencies ++= Seq(
      // main
      "com.julianpeeters" %%% "dc10-core" % Dc10V,
      // test
      "org.scalameta"     %% "munit"      % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val metalang = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/metalang"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "dc10-scala-metalang",
    buildInfoKeys := Seq[BuildInfoKey](organization, name, version),
    buildInfoPackage := "dc10.scala.metalang",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
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