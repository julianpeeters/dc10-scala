package dc10.cats.effect

import cats.data.StateT
import dc10.cats.effect.kernel.CatsEffectKernel.Resource
import dc10.scala.{*, given}
import dc10.scala.compiler.{Γ, dep}
import dc10.scala.dsl.{String, Unit, EXTENDS, OBJECT, ==>, apply, dot}

trait CatsEffect[F[_]]:
  type IO[_]
  def IO: `Type: x_x`[IO]
  def IOApp(name: String)(run: F[`Value.Val: x_x x`[IO, Unit]]): F[Unit]
  def Run(program: `Value: x_x x`[IO, Unit]): F[`Value.Val: x_x x`[IO, Unit]]
  extension (io: `Type: x_x`[IO])
    def println(msg: `Value: x`[String]): `Value.AppDot.1: x_x x`[IO, String, Unit]
  extension [A] (io: `Value: x_x x`[IO, A])
    def toResource: `Value: lx_xl_x_x x_x x`[Resource, IO, A]
  extension [G[_[_], _], H[_], A] (io: `Value: x_x llx_xl_x_x x_x xl`[IO, G, H, A])
    def toResource: `Value: lx_xl_x_x x_x llx_xl_x_x x_x xl`[Resource, IO, G, H, A]


object CatsEffect:

  val lib: LibDep = LibDep("org.typelevel", "cats-effect", "3.5.4")

  val impl: CatsEffect[[A] =>> StateT[ErrorF, Γ, A]] =
    new CatsEffect[[A] =>> StateT[ErrorF, Γ, A]]:

      def IO: `Type: x_x`[IO] =
        `Type.Var: x_x`(0, AliasSym("cats.effect.IO"), None)

      def IOApp(
        name: String
      )(
        run: StateT[ErrorF, Γ, `Value.Val: x_x x`[IO, Unit]]
      ): StateT[ErrorF, Γ, Unit] =
        for
          _ <- OBJECT"$name".EXTENDS(`Type.Var: x`(0, AliasSym("cats.effect.IOApp.Simple"), None)) {run}
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
        yield ()

      def Run(program: `Value: x_x x`[IO, Unit]): StateT[ErrorF, Γ, `Value.Val: x_x x`[IO, Unit]] =
        for
          v <- StateT.pure(`Value.Val: x_x x`(0, `ValSym`("run"), program.tpe, Some(program)))
          d <- StateT.pure(`ValDef: x_x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    
      extension (io: `Type: x_x`[IO])
        def println(msg: `Value: x`[String]): `Value.AppDot.1: x_x x`[IO, String, Unit] =
          io.dot(
            `Value.Def.1: x_x_x x lx_x xl`(
              0,
              `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
              msg.tpe ==> io(Unit),
              None
            )
          )(msg)

  
          // `Value.AppDot.1: x_x x`(
          //   0,
          //   `Value.Def.1: x_x_x x lx_x xl`(
          //     0,
          //     `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
          //     msg.tpe ==> io(Unit),
          //     None
          //   ),
          //   io,
          //   msg,
          //   io(Unit)
          // )
          // `Value.Def.1: x_x_x x lx_x xl`(
          //   0,
          //   `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
          //   msg.tpe ==> io(Unit),
          //   None
          // )
          // .apply(msg)

      extension [A] (io: `Value: x_x x`[IO, A])
        def toResource: `Value: lx_xl_x_x x_x x`[Resource, IO, A] =
          io.dot(
            `Value.Def.0: x_x_x lx_x xl llx_xl_x_x x_x xl`(
              0,
              `DefSym.0`("toResource"),
              io.tpe ==> `Type.App: lx_xl_x_x x_x x`(0, Resource, IO, io.tpe.targ1),
              None
            )
          )

      extension [G[_[_],_], H[_], A](io: `Value: x_x llx_xl_x_x x_x xl`[IO, G, H, A])
        def toResource: `Value: lx_xl_x_x x_x llx_xl_x_x x_x xl`[Resource, IO, G, H, A] =
          io.dot(
            `Value.Def.0: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`(
              0,
              `DefSym.0`("toResource"),
              io.tpe ==> `Type.App: lx_xl_x_x x_x llx_xl_x_x x_x xl`(0, Resource, IO, io.tpe.targ1),
              None
            )
          )

