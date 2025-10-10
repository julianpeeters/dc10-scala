package dc10.calico

import cats.data.StateT
import dc10.cats.effect.dsl.{IO, Resource}
import dc10.fs2.dom.Dom.HtmlElement
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.dsl.{EXTENDS, OBJECT}

trait IOWebApp[F[_]]:

  def IOWebApp(name: String)(render: F[`Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]]): F[Unit]
  // def Render(program: `Value: x→x x`[IO, Unit]): F[`Value.Val: x→x x`[IO, Unit]]
  def Render(program: `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]): F[`Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]]
  // def Render(program: `Value: x→x x`[IO, Unit]): F[`Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[IO, Unit]]


object IOWebApp:

  val lib: LibDep = LibDep("org.typelevel", "calico", "0.2.3")

  val impl: IOWebApp[[A] =>> StateT[ErrorF, Γ, A]] =
    new IOWebApp[[A] =>> StateT[ErrorF, Γ, A]]:
  
      def IOWebApp(
        name: String
      )(
        render: StateT[ErrorF, Γ, `Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]]
      ): StateT[ErrorF, Γ, Unit] =
        for
          _ <- OBJECT"$name".EXTENDS(`Type.Var: x`(0, AliasSym("calico.IOWebApp"), None)) {render}
          // _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(IOWebApp.lib))
        yield ()

      def Render(
        program: `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]
      ): StateT[ErrorF, Γ, `Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, IO, HtmlElement, IO]] =
        for
          v <- StateT.pure(`Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`(0, `ValSym`("render"), program.tpe, Some(program)))
          d <- StateT.pure(`ValDef: (x→x)→x→x x→x ((x→x)→x x→x)`(v))
          // _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(IOWebApp.lib))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    