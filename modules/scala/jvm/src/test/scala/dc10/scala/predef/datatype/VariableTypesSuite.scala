package dc10.scala.predef.datatype

import _root_.scala.language.implicitConversions
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.5.2`
import munit.FunSuite

class VariableTypesSuite extends FunSuite:

  test("type alias def"):
 
    def ast[A] =
      for
        a <- TYPE("Q") := STRING
        A <- TYPE[A]("A")
        _ <- TYPE("S"):= A
      yield ()
    
    def obtained[A]: String =
      ast.compile.string
      
    val expected: String =
      """|type Q = String
         |type A
         |type S = A""".stripMargin
      
    assertEquals(obtained, expected)

  test("type alias list def"):

    type List[A]
 
    def ast[F[_], A] =
      for
        A <- TYPE[A]("A")
        C <- TYPE[List, __]("List", __)
        _ <- TYPE("Bar") := C(A)


      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|type A
         |type List[_]
         |type Bar = List[A]""".stripMargin
      
    assertEquals(obtained, expected)


  test("type alias stream def"):

    type Stream[F[_], A]
    type Monad[F[_]]
    type IO[A]
 
    def ast[F[_], A] =
      for
        A <- TYPE[A]("A")
        I <- TYPE[IO, A]("IO", TYPE("A"))
        _ <- TYPE[Monad, F]("Monad", TYPE[F, __]("F", __))
        S <- TYPE[Stream, F, A]("Stream", TYPE[F, __]("F", __), TYPE[A]("A"))
        _ <- TYPE("Foo") := S(I, A)
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|type A
         |type IO[A]
         |type Monad[F[_]]
         |type Stream[F[_], A]
         |type Foo = Stream[IO, A]""".stripMargin
      
    assertEquals(obtained, expected)



  test("type lambda def"):
 
    def ast =
      for
        _ <- TYPE("Q") := TYPE("A").==>>[Option](a => OPTION(a))
        _ <- TYPE("A")
      yield ()
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """|type Q = [A] =>> Option[A]
         |type A""".stripMargin
      
    assertEquals(obtained, expected)

  // test("match types def"):
 
  //   def ast = MATCHTYPES("Elem", TYPE("X"), x =>
  //     for
  //       _ <- CASE(STRING ==> STRING) 
  //       _ <- CASE(x ==> x)
  //     yield ()
  //   )
  
  //   val obtained: String =
  //     ast.compile.string
      
  //   val expected: String =
  //     """|type Elem[X] = X match
  //        |  case String => String
  //        |  case X => X""".stripMargin
      
  //   assertEquals(obtained, expected)