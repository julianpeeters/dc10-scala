package dc10.scala

import _root_.scala.language.implicitConversions
import cats.implicits.given
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.7`
import munit.FunSuite

object DEF:
  def apply[F[_], T](v: `Value: x`[T]): Unit = ()

class FunctionsSuite extends FunSuite:

  test("val dec"):

    def ast =
      for
        _ <- VAL"f1"$ Int ==> String
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|val f1: Int => String
         |""".stripMargin
      
    assertEquals(obtained, expected)

  test("val def"):    

    def ast =
      for
        f <- VAL"f"$ String ==> String := ("x"$ String) ==> (s => s)
        b <- VAL"b"$ String := f("hello")
        _ <- VAL"c"$ String := f(b)
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|val f: String => String = x => x
         |val b: String = f("hello")
         |val c: String = f(b)
         |""".stripMargin
      
    assertEquals(obtained, expected)