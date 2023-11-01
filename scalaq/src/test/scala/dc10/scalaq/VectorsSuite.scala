
import _root_.scala.language.implicitConversions
import cats.implicits.given
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`
import dc10.scalaq.dsl.{*, given}
import munit.FunSuite

class VectorsSuite extends FunSuite:
  
  test("vector val dec"):

    def ast =
      for
        f <- VAL("l1", VECTOR(3, INT))
        _ <- VAL("l2", VECTOR(3, STRING))
        _ <- VAL("l3", VECTOR(2, VECTOR(3, STRING)))
        _ <- VAL("l4", VECTOR(2, VECTOR(2, VECTOR(3, STRING))))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
      
    val expected: String =
      """|val l1: List[Int]
         |val l2: List[String]
         |val l3: List[List[String]]
         |val l4: List[List[List[String]]]""".stripMargin
      
    assertEquals(obtained, expected)

  test("vector val def"):
    
    def ast =
      for
        i <- VAL("l1", VECTOR(3, INT), Vector.of(1, 2, 3))
        _ <- VAL("l2", VECTOR(3, STRING), Vector.of("1", "2", "3"))
        l <- VAL("l3", VECTOR(2, VECTOR(3, STRING)), Vector.of(Vector.of("1", "2", "3"), Vector.of("4", "5", "6")))
        _ <- VAL("l4", VECTOR(2, VECTOR(2, VECTOR(3, STRING))), Vector.of(l, l))
        _ <- VAL("l5", VECTOR(6, INT), i ++ i)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
      
    val expected: String =
      """|val l1: List[Int] = List(1, 2, 3)
         |val l2: List[String] = List("1", "2", "3")
         |val l3: List[List[String]] = List(List("1", "2", "3"), List("4", "5", "6"))
         |val l4: List[List[List[String]]] = List(l3, l3)
         |val l5: List[Int] = l1 ++ l1""".stripMargin
      
    assertEquals(obtained, expected)
