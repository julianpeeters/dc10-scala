package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import cats.data.StateT
import dc10.scala.{ErrorF}
import dc10.scala.Statement
import dc10.scala.LibDep
import dc10.scala.Statement.ValueExpr.`Value`
import munit.FunSuite

class PrimitiveTypeSuite extends FunSuite:
  
  // schema
  import dc10.scala.dsl.{*, given}

  // compile
  import dc10.scala.compiler.{compile, toString}
  import dc10.scala.version.`3.3.3`

  test("def dec"):

    def ast =
      for
        _ <- DEF("greeting", VAL("str", STRING), STRING)
        _ <- DEF("chat", STRING)
        _ <- VAL("farewell", STRING)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|def greeting(str: String): String
         |def chat: String
         |val farewell: String""".stripMargin
      
    assertEquals(obtained, expected)

  test("def def"):

    def ast =
      for
        f <- DEF("f", VAL("str", STRING), STRING, s => s)
        _ <- VAL("farewell", STRING, f("aloha"))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|def f(str: String): String = str
         |val farewell: String = f("aloha")""".stripMargin
      
    assertEquals(obtained, expected)

  test("ext def"):

    trait ExtensionR[Z, A]:
      extension (s: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]] | `Value`[A])
        def REPLACE(n: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String]]

    object ExtensionR:
      def apply(f: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String => String]]): ExtensionR[Unit, String] =
        new ExtensionR[Unit, String]:
          extension (s: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String]] | `Value`[String])
            def REPLACE(n: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[String]] =
              s.DOT(f)(n)
        
    def ast =
      for
        given ExtensionR[Unit, String] <- EXT(
          for
            _ <- EXTENSION("str", STRING)
            f <- DEF("replace", VAL("msg", STRING), STRING, b => b)
          yield ExtensionR(f)
        )
        _ <- VAL("farewell", STRING, sLit("hello").REPLACE("goodbye"))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|extension (str: String)
         |  def replace(msg: String): String = msg
         |
         |val farewell: String = "hello".replace("goodbye")""".stripMargin
      
    assertEquals(obtained, expected)

  test("val dec"):

    def ast =
      for
        _ <- VAL("t", BOOLEAN)
        _ <- VAL("f", BOOLEAN)
        _ <- VAL("age", INT)
        _ <- VAL("year", INT)
        _ <- VAL("greeting", STRING)
        _ <- VAL("farewell", STRING)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|val t: Boolean
         |val f: Boolean
         |val age: Int
         |val year: Int
         |val greeting: String
         |val farewell: String""".stripMargin
      
    assertEquals(obtained, expected)

  test("val def"):

    def ast =
      for
        _ <- VAL("t", BOOLEAN, true)
        _ <- VAL("f", BOOLEAN, false)
        _ <- VAL("age", INT, 101)
        _ <- VAL("year", INT, 2020)
        _ <- VAL("greeting", STRING, "hello, world")
        _ <- VAL("farewell", STRING, "goodbye, world")
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|val t: Boolean = true
         |val f: Boolean = false
         |val age: Int = 101
         |val year: Int = 2020
         |val greeting: String = "hello, world"
         |val farewell: String = "goodbye, world"""".stripMargin
      
    assertEquals(obtained, expected)

  test("def dec f"):

    def ast[F[_], A] =
      for
        _ <- DEF("foo", STRING)
        _ <- DEF("bar", TYPE[F, __]("F", __), F => F(STRING))
        _ <- DEF("baz", TYPE[A]("A"), A => A)
        // _ <- DEF("qux", TYPE[F, __]("F", __), TYPE[A]("A"), (F, A) => F(A))
        _ <- VAL("farewell", STRING)
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.3"]
      
    val expected: String =
      """|def foo: String
         |def bar[F[_]]: F[String]
         |def baz[A]: A
         |val farewell: String""".stripMargin
      
    assertEquals(obtained, expected)