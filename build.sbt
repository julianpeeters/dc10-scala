val Dc10V = "0.7.0"
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
    // "-Xkind-projector:underscores",
  ),
  scalaVersion := "3.3.6",
  versionScheme := Some("semver-spec"),
))

lazy val `dc10-calico` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/calico"))
  .settings(
    name := "dc10-calico",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .dependsOn(`dc10-fs2`, `dc10-fs2-dom`)

lazy val `dc10-cats-effect` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/catseffect"))
  .settings(
    name := "dc10-cats-effect",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})
  .dependsOn(`dc10-scala`)

lazy val `dc10-fs2` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/fs2"))
  .settings(
    name := "dc10-fs2",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})
  .dependsOn(`dc10-cats-effect`)

lazy val `dc10-fs2-dom` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/fs2dom"))
  .settings(
    name := "dc10-fs2-dom",
    libraryDependencies ++= Seq(
      // main
      //
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .dependsOn(`dc10-scala`)

lazy val `dc10-sbt` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/sbt"))
  .settings(
    name := "dc10-sbt",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % MUnitV % Test
    )
  )
  .jsSettings(test := {})
  .nativeSettings(test := {})
  .dependsOn(`dc10-scala`)

lazy val `dc10-scala` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("modules/scala"))
  .settings(
    name := "dc10-scala",
    libraryDependencies ++= Seq(
      // main
      "com.julianpeeters" %%% "dc10" % Dc10V,
      // test
      "org.scalameta" %% "munit" % MUnitV % Test
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
  .dependsOn(`dc10-scala`)
  .jsSettings(test := {})
  .nativeSettings(test := {})

lazy val docs = project.in(file("docs/gitignored"))
  .settings(
    mdocOut := file("."),
    mdocVariables := Map(
      "SCALA" -> crossScalaVersions.value.map(e => e.takeWhile(_ != '.')).mkString(", "),
      "VERSION" -> version.value.takeWhile(_ != '+'),
    ),
    test := {}
  )
  .dependsOn(`dc10-cats-effect`.jvm, `dc10-sbt`.jvm, `dc10-scala`.jvm)
  .enablePlugins(MdocPlugin)
  .enablePlugins(NoPublishPlugin)