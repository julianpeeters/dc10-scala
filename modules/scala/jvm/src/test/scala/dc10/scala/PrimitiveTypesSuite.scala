import _root_.scala.language.implicitConversions
import dc10.scala.* 
import munit.FunSuite

class PrimitiveTypeSuite extends FunSuite:
  
  // schema
  import dc10.scala.dsl.{*, given}

  // compile
  import dc10.scala.compiler.{compile, string}
  import dc10.scala.version.`3.3.6`

  test("def dec"):

    def ast =
      for
        _ <- DEF"greeting"("str"$ String)$ String 
        _ <- DEF"chat"$ String
        _ <- VAL"farewell"$ String
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|def greeting(str: String): String
         |def chat: String
         |val farewell: String
         |""".stripMargin
      
    assertEquals(obtained, expected)

  test("def def"):

    def ast =
      for
        f <- DEF"f"("str"$ String)$ String := (s => s)
        _ <- VAL"farewell"$ String := f("aloha")
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|def f(str: String): String = str
         |val farewell: String = f("aloha")
         |""".stripMargin
      
    assertEquals(obtained, expected)

  test("val dec"):

    def ast =
      for
        _ <- VAL"t"$ Boolean
        _ <- VAL"f"$ Boolean
        _ <- VAL"age"$ Int
        _ <- VAL"year"$ Int
        _ <- VAL"greeting"$ String
        _ <- VAL"farewell"$ String
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|val t: Boolean
         |val f: Boolean
         |val age: Int
         |val year: Int
         |val greeting: String
         |val farewell: String
         |""".stripMargin
      
    assertEquals(obtained, expected)