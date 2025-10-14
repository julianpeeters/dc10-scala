package dc10.fs2.dom

// import cats.data.StateT
// import dc10.cats.effect.kernel.CatsEffectKernel.Resource
import dc10.scala.*
// import dc10.scala.compiler.{Γ, dep}
// import dc10.scala.dsl.{String, Unit, EXTENDS, OBJECT, ==>, apply, dot}


object Dom:

  type HtmlElement[_[_]]

  val lib: LibDep = LibDep("org.typelevel", "fs2-dom", "0.2.1")

  def HtmlElement: `Type: lx_xl_x`[HtmlElement] =
    `Type.Var: lx_xl_x`(0, AliasSym("fs2.dom.HtmlElement"), None)
