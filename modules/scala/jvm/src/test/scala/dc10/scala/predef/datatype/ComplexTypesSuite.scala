package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import cats.implicits.given
import munit.FunSuite

import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.4.0`

class ComplexTypesSuite extends FunSuite:
  
  test("list val dec"):

    def ast =
      for
        _ <- VAL("l1", LIST(INT))
        _ <- VAL("l2", LIST(STRING))
        _ <- VAL("l3", LIST(LIST(STRING)))
        _ <- VAL("l4", LIST(LIST(LIST(STRING))))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val l1: List[Int]
         |val l2: List[String]
         |val l3: List[List[String]]
         |val l4: List[List[List[String]]]""".stripMargin
      
    assertEquals(obtained, expected)

  test("list val def"):
    
    def ast =
      for
        _ <- VAL("l0", LIST(INT), List())
        _ <- VAL("l1", LIST(INT), List(1, 2, 3))
        a <- VAL("l2", LIST(STRING), List("1", "2", "3"))
        l <- VAL("l3", LIST(LIST(STRING)), List(List("1", "2", "3"), List("4", "5", "6")))
        _ <- VAL("l4", LIST(LIST(LIST(STRING))), List(l, l))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val l0: List[Int] = List()
         |val l1: List[Int] = List(1, 2, 3)
         |val l2: List[String] = List("1", "2", "3")
         |val l3: List[List[String]] = List(List("1", "2", "3"), List("4", "5", "6"))
         |val l4: List[List[List[String]]] = List(l3, l3)""".stripMargin
      
    assertEquals(obtained, expected)

  test("option val dec"):

    def ast =
      for
        _ <- VAL("l1", OPTION(INT))
        _ <- VAL("l2", OPTION(STRING))
        _ <- VAL("l3", OPTION(OPTION(STRING)))
        _ <- VAL("l4", OPTION(OPTION(OPTION(STRING))))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val l1: Option[Int]
         |val l2: Option[String]
         |val l3: Option[Option[String]]
         |val l4: Option[Option[Option[String]]]""".stripMargin
      
    assertEquals(obtained, expected)

  test("option val def"):
    
    def ast =
      for
        s <- VAL("s1", OPTION(INT), Some(1))
        _ <- VAL("l1", OPTION(INT), Option(1))
        a <- VAL("l2", OPTION(STRING), Option("1"))
        l <- VAL("l3", OPTION(OPTION(STRING)), Option(Option("1")))
        _ <- VAL("l4", OPTION(OPTION(OPTION(STRING))), Option(l))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val s1: Option[Int] = Some(1)
         |val l1: Option[Int] = Option(1)
         |val l2: Option[String] = Option("1")
         |val l3: Option[Option[String]] = Option(Option("1"))
         |val l4: Option[Option[Option[String]]] = Option(l3)""".stripMargin
      
    assertEquals(obtained, expected)

  test("tuple val def"):
    
    def ast =
      for
        s <- VAL("s1", TUPLE(INT, STRING))
        _ <- VAL("s2", TUPLE(INT, STRING), Tuple(1, "hello"))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|val s1: Tuple2[Int, String]
         |val s2: Tuple2[Int, String] = (1, "hello")""".stripMargin
      
    assertEquals(obtained, expected)
