package dc10.scala.predef.datatype

import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.4.0`
import munit.FunSuite
import scala.language.implicitConversions

class TemplateTypesSuite extends FunSuite:

  test("case class 1 def"):

    type Person1

    def ast = CASECLASS[Person1, String]("Person1", FIELD("name", STRING))
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """case class Person1(name: String)""".stripMargin
      
    assertEquals(obtained, expected)


  test("case class 2 def"):

    type Person2

    def ast =
      CASECLASS[Person2, String, Int]("Person2",
        for
          name <- FIELD("name", STRING)
          age <- FIELD("age", INT)
        yield (name, age)
      )
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|case class Person2(
         |  name: String,
         |  age: Int,
         |)""".stripMargin
      
    assertEquals(obtained, expected)

  test("trait Foo"):

    type Foo

    def ast =
      TRAIT[Foo]("Foo",
        for
          _ <- DEF("name", VAL("s", STRING), STRING)
        yield ()
      )
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
    
    val expected: String =
      """|trait Foo:
         |  def name(s: String): String""".stripMargin
      
    assertEquals(obtained, expected)

  test("trait Bar"):

    type Bar[F[_]]

    def ast[F[_], A] =
      TRAIT[Bar, F, __]("Bar", TYPE[F, __]("F", __), F =>
        for
          _ <- DEF("name", VAL("s", STRING), F(STRING))
          _ <- DEF("age", VAL("s", INT), F(INT))
        yield ()
      )
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
    
    val expected: String =
      """|trait Bar[F[_]]:
         |  def name(s: String): F[String]
         |  def age(s: Int): F[Int]""".stripMargin
      
    assertEquals(obtained, expected)