package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.4.0`
import munit.FunSuite

class VariableTypesSuite extends FunSuite:

  test("type alias def"):

    type Stream[_[_], _]
 
    def ast[F[_], A] =
      for
        _ <- TYPE("Q") := STRING
        A <- TYPE[A]("A")
        _ <- TYPE("S"):= A
        F <- TYPE[F, __]("Functor", __)
        _ <- TYPE[Stream, F, A]("Stream", TYPE[F, __]("F", __), TYPE[A]("A"))
        // _ <- TYPE("Foo") := S(F, A)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|type Q = String
         |type A
         |type S = A
         |type Functor[_]
         |type Stream[F[_], A]""".stripMargin
      
    assertEquals(obtained, expected)

  test("match types def"):
 
    def ast = MATCHTYPES("Elem", TYPE("X"), x =>
      for
        _ <- CASE(STRING ==> STRING) 
        _ <- CASE(x ==> x)
      yield ()
    )
  
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      """|type Elem[X] = X match
         |  case String => String
         |  case X => X""".stripMargin
      
    assertEquals(obtained, expected)