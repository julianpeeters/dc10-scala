package dc10.calico.html

import dc10.fs2.dom.Dom.HtmlElement
import dc10.cats.effect.dsl.{IO, Resource}
import dc10.fs2.concurrent.Concurrent.SignallingRef
import dc10.scala.*
import dc10.scala.dsl.*

object io:

  def div(
    arg: `Value.Val: (x→x)→x→x x→x x`[SignallingRef, IO, String]
  ): `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO] =
    `Value.Def.1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`(
      0,
      `DefSym.1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`("div", `Value.Val: (x→x)→x→x x→x x`(0, ValSym("arg"), arg.tpe, None)),
      arg.tpe ==> Resource(IO, HtmlElement(IO)),
      None
    ).apply(arg)