package dc10.cats.effect

import cats.data.StateT
import dc10.cats.effect.kernel.CatsEffectKernel.Resource
import dc10.scala.{*, given}
import dc10.scala.compiler.{Γ, dep}
import dc10.scala.dsl.{String, Unit, EXTENDS, OBJECT, ==>, apply, dot}


trait CatsEffect[F[_]]:
  type IO[_]
  def IO: `Type: x→x`[IO]
  def IOApp(name: String)(run: F[`Value.Val: x→x x`[IO, Unit]]): F[Unit]
  def Run(program: `Value: x→x x`[IO, Unit]): F[`Value.Val: x→x x`[IO, Unit]]
  extension (io: `Type: x→x`[IO])
    def println(msg: `Value: x`[String]): `Value.AppDot.1: x→x x`[IO, String, Unit]
  extension [A] (io: `Value: x→x x`[IO, A])
    def toResource: `Value: (x→x)→x→x x→x x`[Resource, IO, A]
  extension [G[_[_], _], H[_], A] (io: `Value: x→x ((x→x)→x→x x→x x)`[IO, G, H, A])
    def toResource: `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[Resource, IO, G, H, A]


object CatsEffect:

  val lib: LibDep = LibDep("org.typelevel", "cats-effect", "3.5.4")

  val impl: CatsEffect[[A] =>> StateT[ErrorF, Γ, A]] =
    new CatsEffect[[A] =>> StateT[ErrorF, Γ, A]]:

      def IO: `Type: x→x`[IO] =
        `Type.Var: x→x`(0, AliasSym("cats.effect.IO"), None, ctors = () => Nil)

      def IOApp(
        name: String
      )(
        run: StateT[ErrorF, Γ, `Value.Val: x→x x`[IO, Unit]]
      ): StateT[ErrorF, Γ, Unit] =
        for
          _ <- OBJECT"$name".EXTENDS(`Type.Var: x`(0, AliasSym("cats.effect.IOApp.Simple"), None)) {run}
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
        yield ()

      def Run(program: `Value: x→x x`[IO, Unit]): StateT[ErrorF, Γ, `Value.Val: x→x x`[IO, Unit]] =
        for
          v <- StateT.pure(`Value.Val: x→x x`(0, `ValSym`("run"), program.tpe, Some(program)))
          d <- StateT.pure(`ValDef: x→x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    
      extension (io: `Type: x→x`[IO])
        def println(msg: `Value: x`[String]): `Value.AppDot.1: x→x x`[IO, String, Unit] =
          io.dot(
            `Value.Def.1: x→x→x x (x→x x)`(
              0,
              `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
              msg.tpe ==> io(Unit),
              None
            )
          )(msg)

  
          // `Value.AppDot.1: x→x x`(
          //   0,
          //   `Value.Def.1: x→x→x x (x→x x)`(
          //     0,
          //     `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
          //     msg.tpe ==> io(Unit),
          //     None
          //   ),
          //   io,
          //   msg,
          //   io(Unit)
          // )
          // `Value.Def.1: x→x→x x (x→x x)`(
          //   0,
          //   `DefSym.1`("println", `Value.Val: x`(0, ValSym("msg"), String, None)),
          //   msg.tpe ==> io(Unit),
          //   None
          // )
          // .apply(msg)

      extension [A] (io: `Value: x→x x`[IO, A])
        def toResource: `Value: (x→x)→x→x x→x x`[Resource, IO, A] =
          io.dot(
            `Value.Def.0: x→x→x (x→x x) ((x→x)→x→x x→x x)`(
              0,
              `DefSym.0`("toResource"),
              io.tpe ==> `Type.App: (x→x)→x→x x→x x`(0, Resource, IO, io.tpe.targ1),
              None
            )
          )

      extension [G[_[_],_], H[_], A](io: `Value: x→x ((x→x)→x→x x→x x)`[IO, G, H, A])
        def toResource: `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[Resource, IO, G, H, A] =
          io.dot(
            `Value.Def.0: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`(
              0,
              `DefSym.0`("toResource"),
              io.tpe ==> `Type.App: (x→x)→x→x x→x ((x→x)→x→x x→x x)`(0, Resource, IO, io.tpe.targ1),
              None
            )
          )

