package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import cats.implicits.*
import dc10.compiler.{Compiler, compile, toString}
import munit.FunSuite

import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.0`
import dc10.scala.dsl

class ComplexTypesSuite extends FunSuite:

  test("case class def"):

    type Person

    def ast = CASECLASS[Person, String]("Person", VAL("name", STRING))
    
    val obtained: Either[List[Compiler.Error], String] =
      ast.compile.toString["scala-3.3.0"]
      
    val expected: Either[List[Compiler.Error], String] =
      Right("""case class Person(val name: String)""".stripMargin)
      
    assertEquals(obtained, expected)
  
  test("list val dec"):

    def ast =
      for
        _ <- VAL("l1", LIST(INT))
        _ <- VAL("l2", LIST(STRING))
        _ <- VAL("l3", LIST(LIST(STRING)))
        _ <- VAL("l4", LIST(LIST(LIST(STRING))))
      yield ()
    
    val obtained: Either[List[Compiler.Error], String] =
      ast.compile.toString["scala-3.3.0"]
      
    val expected: Either[List[Compiler.Error], String] =
      Right("""|val l1: List[Int]
               |val l2: List[String]
               |val l3: List[List[String]]
               |val l4: List[List[List[String]]]""".stripMargin
      )
      
    assertEquals(obtained, expected)

  test("list val def"):
    
    def ast =
      for
        _ <- VAL("l1", LIST(INT))(List(1, 2, 3))
        a <- VAL("l2", LIST(STRING))(List("1", "2", "3"))
        l <- VAL("l3", LIST(LIST(STRING)))(List(List("1", "2", "3"), List("4", "5", "6")))
        _ <- VAL("l4", LIST(LIST(LIST(STRING))))(List(l, l))
      yield ()
    
    val obtained: Either[List[Compiler.Error], String] =
      ast.compile.toString["scala-3.3.0"]
      
    val expected: Either[List[Compiler.Error], String] =
      Right("""|val l1: List[Int] = List(1, 2, 3)
               |val l2: List[String] = List("1", "2", "3")
               |val l3: List[List[String]] = List(List("1", "2", "3"), List("4", "5", "6"))
               |val l4: List[List[List[String]]] = List(l3, l3)""".stripMargin
      )
      
    assertEquals(obtained, expected)
