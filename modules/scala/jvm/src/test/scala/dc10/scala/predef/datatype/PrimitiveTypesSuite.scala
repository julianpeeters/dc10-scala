import _root_.scala.language.implicitConversions
import cats.data.StateT
import dc10.scala.* 
import munit.FunSuite

class PrimitiveTypeSuite extends FunSuite:
  
  // schema
  import dc10.scala.dsl.{*, given}

  // compile
  import dc10.scala.compiler.{compile, string}
  import dc10.scala.version.`3.5.2`

  // test("def dec"):

  //   def ast =
  //     for
  //       _ <- DEF("greeting", VAL("str", STRING), STRING)
  //       _ <- DEF("chat", STRING)
  //       _ <- VAL("farewell", STRING)
  //     yield ()
    
  //   val obtained: String =
  //     ast.compile.string
      
  //   val expected: String =
  //     """|def greeting(str: String): String
  //        |def chat: String
  //        |val farewell: String""".stripMargin
      
  //   assertEquals(obtained, expected)

  test("def def"):

    def ast =
      for
        f <- DEF("f", VAL("str", STRING), STRING) := (s => s)
        _ <- VAL("farewell", STRING) := f("aloha")
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|def f(str: String): String = str
         |val farewell: String = f("aloha")""".stripMargin
      
    assertEquals(obtained, expected)

  test("ext def"):

    case class ExtensionR(
      f: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String => String]],
      g: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String => String]]
    ):
      extension (s: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]])
        def REPLACE(n: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]] =
          s.DOT(f)(n)
        def REPLACE2(n: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]] =
          s.DOT(g)(n)
        
    def ast =
      for
        given ExtensionR <- EXT("str", STRING)(
          for
            f <- DEF("replace", VAL("msg", STRING), STRING) := (a => a)
            g <- DEF("replace2", VAL("msg", STRING), STRING) := (a => a)
          yield ExtensionR(f, g)
        )
        _ <- VAL("farewell", STRING) := sLit("hello").REPLACE("goodbye")
        _ <- VAL("aloha", STRING) := sLit("hello").REPLACE2("aloha")
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|extension (str: String)
         |  def replace(msg: String): String = msg
         |  def replace2(msg: String): String = msg
         |
         |val farewell: String = "hello".replace("goodbye")
         |val aloha: String = "hello".replace2("aloha")""".stripMargin
      
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
      ast.compile.string
      
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
        _ <- VAL("t", BOOLEAN) := true
        _ <- VAL("f", BOOLEAN) := false
        _ <- VAL("age", INT) := 101
        _ <- VAL("year", INT) := 2020
        _ <- VAL("greeting", STRING) := "hello, world"
        _ <- VAL("farewell", STRING) := "goodbye, world"
      yield ()
    
    val obtained: String =
      ast.compile.string
      
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
      ast.compile.string
      
    val expected: String =
      """|def foo: String
         |def bar[F[_]]: F[String]
         |def baz[A]: A
         |val farewell: String""".stripMargin
      
    assertEquals(obtained, expected)