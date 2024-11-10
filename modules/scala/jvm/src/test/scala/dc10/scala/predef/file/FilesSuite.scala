package dc10.scala.predef.file

import _root_.scala.language.implicitConversions
import dc10.scala.{Error, dsl}
import munit.FunSuite

class PrimitiveTypeSuite extends FunSuite:
  
  // schema
  import dc10.scala.dsl.* 

  // compile
  import dc10.scala.compiler.{compile, virtualFile}
  import dc10.scala.version.`3.3.4`

  test("val dec"):

    def ast = FILE("test.scala",
      for
        _ <- VAL("t", BOOLEAN)
        _ <- VAL("f", BOOLEAN)
        _ <- VAL("age", INT)
        _ <- VAL("year", INT)
        _ <- VAL("greeting", STRING)
        _ <- VAL("farewell", STRING)
      yield ()
    )
    val obtained: Either[List[Error], List[String]] =
      ast.compile
         .virtualFile
         .map(fs => fs.map(vf => vf.contents))
      
    val expected: Either[List[Error], List[String]] =
      Right(scala.List("""|val t: Boolean
                          |val f: Boolean
                          |val age: Int
                          |val year: Int
                          |val greeting: String
                          |val farewell: String""".stripMargin
      ))
      
    assertEquals(obtained, expected)

  