package dc10.scala.predef

import _root_.scala.language.implicitConversions
import cats.implicits.given
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.{LibDep, Statement}
import dc10.scala.version.`3.3.4`
import munit.FunSuite

class FunctionsSuite extends FunSuite:

  test("val dec"):

    def ast: cats.data.StateT[dc10.scala.ErrorF, (Set[LibDep], List[Statement]), Unit] =
      for
        _ <- VAL("f1", INT ==> STRING)
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """val f1: Int => String""".stripMargin
      
    assertEquals(obtained, expected)

  test("val def"):
    
    def ast =
      for
        f <- VAL("f1", STRING ==> STRING,
          VAL("input", STRING) ==> (s => s)
        )
        b <- VAL("b", STRING, f("hello"))
        _ <- VAL("c", STRING, f(b))
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|val f1: String => String = input => input
         |val b: String = f1("hello")
         |val c: String = f1(b)""".stripMargin
      
    assertEquals(obtained, expected)

  test("for"):
    
    def ast = VAL("f1", OPTION(STRING),
        FOR(
          for
            s <- "s" <-- Option("wowie")
            t <- "t" <-- Option(s)
          yield t
        )
    )
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|val f1: Option[String] = 
         |  for
         |    s <- Option("wowie")
         |    t <- Option(s)
         |  yield t""".stripMargin
      
    assertEquals(obtained, expected)