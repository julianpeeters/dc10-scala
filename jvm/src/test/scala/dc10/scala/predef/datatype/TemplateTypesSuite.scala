package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.*
import dc10.scala.version.`3.4.0`
import munit.FunSuite

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
      CASECLASS[Person2, String, Int]( "Person2",
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