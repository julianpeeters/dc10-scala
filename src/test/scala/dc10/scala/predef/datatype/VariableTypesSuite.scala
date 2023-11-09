package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import cats.implicits.*
import dc10.scala.compiler.{compile, toString}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.1`
import munit.FunSuite

class VariableTypesSuite extends FunSuite:

  test("type alias def"):
 
    def ast =
      for
        s <- TYPE("S", STRING)
        _ <- TYPE("T", s)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
      
    val expected: String =
      """|type S = String
         |type T = S""".stripMargin
      
    assertEquals(obtained, expected)

  test("match types def"):
 
    def ast = MATCHTYPES("Elem", TYPE("X"), x =>
      for
        _ <- CASE(STRING ==> STRING) 
        _ <- CASE(x ==> x)
      yield ()
    )
  
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
      
    val expected: String =
      """|type Elem[X] = X match
         |  case String => String
         |  case X => X""".stripMargin
      
    assertEquals(obtained, expected)