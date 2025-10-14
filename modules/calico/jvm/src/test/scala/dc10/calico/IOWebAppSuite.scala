package dc10.calico

import _root_.scala.language.implicitConversions
import dc10.calico.dsl.{IOWebApp, Render, div}
import dc10.cats.effect.dsl.IO
import dc10.fs2.concurrent.dsl.SignallingRef
import dc10.scala.compiler.{compile, string}
import dc10.scala.dsl.{*, given}
import dc10.scala.version.`3.3.6`

import munit.FunSuite

class IOWebAppSuite extends FunSuite:

  test("io web app"):

    def ast =
      IOWebApp("Main"):
        Render(
          SignallingRef(IO).of("World").toResource.flatMap { ("name"$ SignallingRef(IO, String)) ==> (name =>
              div(name)
            )
          }
        )
    
    val obtained: String =
      ast.compile.string
      
    val expected: String =
      """object Main extends calico.IOWebApp:
        |
        |  val render: cats.effect.Resource[cats.effect.IO, fs2.dom.HtmlElement[cats.effect.IO]] = fs2.concurrent.SignallingRef[cats.effect.IO].of("World").toResource.flatMap(name => div(name))
        |
        |""".stripMargin
      
    assertEquals(obtained, expected)
