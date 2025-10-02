package dc10.dsl.predef.datatype

import munit.FunSuite

class SbtSuite extends FunSuite:
  
  import dc10.sbt.dsl.*
  import dc10.sbt.compiler
  import dc10.sbt.version.`1.11.6`
  import dc10.scala.dsl.{*, given}
  import dc10.scala.version.`3.3.6`
  import scala.language.implicitConversions
  
  test("base dir"):

    val `Main.scala` =
      FILE"Main.scala" {
        PACKAGE"example" {
          VAL"hello"$ String := "hello, world"
        }
      }

    val ast =
      BASEDIR"dc10-example".withSelf: self => 
        for
          s <- SRC(`Main.scala`)
          _ <- BUILD(root(self.nme, s))
        yield ()

    val obtained: List[String] =
      ast.compile.virtualFile.fold(
        es => es.map(e => e.toString),
        l => l.filter(f => f.path.toString().contains("build.sbt")).map(f => f.contents)
      )
      
    val expected: List[String] =
      List(
        """ThisBuild / scalaVersion := "3.3.6"
          |ThisBuild / version := "0.1.0-SNAPSHOT"
          |
          |lazy val root = (project in file(".")).settings(
          |  name := "dc10-example",
          |  libraryDependencies ++= Seq(
          |    
          |  )
          |)""".stripMargin
      )

    assertEquals(obtained, expected)

  // test("base dir crossproject"):

  //   def `Main.scala` =
  //     file("Main.scala")(
  //       package_"example" (
  //         VAL"hello"$ String := "hello, world"
  //       )
  //     )

  //   val ast =
  //     repo("dc10-example").withSelf: self =>
  //       for
  //         s <- src(`Main.scala`)
  //         _ <- build(crossProject(self.nme, s))
  //         _ <- readme("## `dc10-example`")
  //       yield ()

    
  //   val obtained: List[String] =
  //     ast.compile.virtualFile.fold(_ => Nil, l => l.map(f => f.contents))
      
  //   val expected: List[String] =
  //     scala.List(
  //       """|package example
  //          |
  //          |val hello: String = "hello, world"""".stripMargin,
  //       """|ThisBuild / scalaVersion := "3.3.6"
  //          |ThisBuild / version := "0.1.0-SNAPSHOT"
  //          |
  //          |lazy val `dc10-example` = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  //          |  .in(file("."))
  //          |  .settings(
  //          |    name := "dc10-example",
  //          |    libraryDependencies ++= Seq(
  //          |       
  //          |    )
  //          |)""".stripMargin,
  //       """## `dc10-example`""".stripMargin,
  //       """|addSbtPlugin("org.portable-scala" %% "sbt-scalajs-crossproject" % "1.3.2")
  //          |
  //          |addSbtPlugin("org.portable-scala" %% "sbt-scala-native-crossproject" % "1.3.2")
  //          |
  //          |addSbtPlugin("org.scala-js" %% "sbt-scalajs" % "1.17.0")
  //          |
  //          |addSbtPlugin("org.scala-native" %% "sbt-scala-native" % "0.5.6")""".stripMargin
  //     )

  //   assertEquals(obtained, expected)

  