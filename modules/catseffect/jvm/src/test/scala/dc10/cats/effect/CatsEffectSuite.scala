
import _root_.scala.language.implicitConversions
import dc10.cats.effect.dsl.{IO, IOApp, println}
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.6`

import munit.FunSuite

class CatsEffectSuite extends FunSuite:

  test("io val"):

    def ast = VAL"foo"$ IO(Unit) := IO.println("Hello, World!")
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      "val foo: cats.effect.IO[Unit] = cats.effect.IO.println(\"Hello, World!\")\n".stripMargin
      
    assertEquals(obtained, expected)
  
  test("ioapp val"):

    def ast =
      IOApp("HelloWorld"):
        VAL"run"$ IO(Unit) := IO.println("Hello, World!")
    
    val obtained: String =
      ast.compile.string  
      
    val expected: String =
      """object HelloWorld extends cats.effect.IOApp.Simple:
        |
        |  val run: cats.effect.IO[Unit] = cats.effect.IO.println("Hello, World!")
        |
        |  """.stripMargin
    
    assertEquals(obtained, expected.dropRight(2))