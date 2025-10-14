package dc10.calico.html

import dc10.fs2.dom.Dom.HtmlElement
import dc10.cats.effect.dsl.{IO, Resource}
import dc10.fs2.concurrent.Concurrent.SignallingRef
import dc10.scala.*
import dc10.scala.dsl.*

object io:

  def div(
    arg: `Value.Val: lx_xl_x_x x_x x`[SignallingRef, IO, String]
  ): `Value: lx_xl_x_x x_x llx_xl_x x_xl`[Resource, IO, HtmlElement, IO] =
    `Value.Def.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(
      0,
      `DefSym.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`("div", `Value.Val: lx_xl_x_x x_x x`(0, ValSym("arg"), arg.tpe, None)),
      arg.tpe ==> Resource(IO, HtmlElement(IO)),
      None
    ).apply(arg)