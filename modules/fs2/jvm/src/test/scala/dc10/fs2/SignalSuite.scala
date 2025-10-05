package dc10.fs2

import _root_.scala.language.implicitConversions
import dc10.cats.effect.dsl.IO
import dc10.fs2.concurrent.dsl.SignallingRef
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.6`

import munit.FunSuite

class SignalSuite extends FunSuite:

  test("signalling ref"):

    def ast = VAL"foo"$ IO(SignallingRef(IO, String)) := SignallingRef(IO).of("Hello, World")
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      "val foo: cats.effect.IO[fs2.concurrent.SignallingRef[cats.effect.IO, String]] = fs2.concurrent.SignallingRef[cats.effect.IO].of(\"Hello, World\")\n".stripMargin
      
    assertEquals(obtained, expected)
