package dc10.cats.effect.kernel

import dc10.cats.effect.dsl.*
import dc10.cats.effect.kernel.dsl.Resource
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.7`
import scala.language.implicitConversions

import munit.FunSuite

class CatsEffectKernelSuite extends FunSuite:

  test("io toResource"):

    def ast =
      VAL"foo"$ Resource(IO, Unit) :=
        IO.println("Hello, World!").toResource

    val obtained: String =
      ast.compile.string

    val expected: String =
      "val foo: cats.effect.Resource[cats.effect.IO, Unit] = cats.effect.IO.println(\"Hello, World!\").toResource\n".stripMargin
      
    assertEquals(obtained, expected)