package dc10.cats.effect

import cats.data.StateT
import dc10.cats.effect.kernel.CatsEffectKernel.Resource
import dc10.scala.{*, given}
import dc10.scala.compiler.{Γ, dep}
import dc10.scala.dsl.{String, Unit, EXTENDS, OBJECT, ==>, apply, dot}


trait CatsEffect[F[_]]:
  type IO[_]
  type IOAPP
  def IO: `Type.Expr: *→*`[IO]
  def IOApp(name: String)(run: F[`Value.Val: *→* *`[IO, Unit]]): F[Unit]
  def Run(program: `Value.Expr: *→* *`[IO, Unit]): F[`Value.Val: *→* *`[IO, Unit]]
  extension (io: `Type.Expr: *→*`[IO])
    def println(msg: `Value.Expr: *`[String]): `Value.App.1: *→* *`[IO, String, Unit]
  extension [A] (io: `Value.Expr: *→* *`[IO, A])
    def toResource: `Value.Expr: (*→*)→*→* *→* *`[Resource, IO, A]

object CatsEffect:

  val lib: LibDep = LibDep("org.typelevel", "cats-effect", "3.5.4")

  val impl: CatsEffect[[A] =>> StateT[ErrorF, Γ, A]] =
    new CatsEffect[[A] =>> StateT[ErrorF, Γ, A]]:

      def IO: `Type.Expr: *→*`[IO] =
        `Type.Var: *→*`(0, AliasSym("cats.effect.IO"), None, ctors = () => Nil)

      def IOApp(
        name: String
      )(
        run: StateT[ErrorF, Γ, `Value.Val: *→* *`[IO, Unit]]
      ): StateT[ErrorF, Γ, Unit] =
        for
          _ <- OBJECT"$name".EXTENDS(`Type.Var: *`(0, AliasSym("cats.effect.IOApp.Simple"), None)) {run}
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
        yield ()

      def Run(program: `Value.Expr: *→* *`[IO, Unit]): StateT[ErrorF, Γ, `Value.Val: *→* *`[IO, Unit]] =
        for
          v <- StateT.pure(`Value.Val: *→* *`(0, `ValSym`("run"), program.tpe, Some(program)))
          d <- StateT.pure(`ValDef: *→* *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(CatsEffect.lib))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    
      extension (io: `Type.Expr: *→*`[IO])
        def println(msg: `Value.Expr: *`[String]): `Value.App.1: *→* *`[IO, String, Unit] =
          `Value.Def.1: *→*→* * (*→* *)`(
            0,
            `DefSym.1`("cats.effect.IO.println", `Value.Val: *`(0, ValSym("msg"), String, None)),
            msg.tpe ==> `Type.App: *→* *`(0, IO, Unit),
            None
          ).apply(msg)

      extension [A] (io: `Value.Expr: *→* *`[IO, A])
        def toResource: `Value.Expr: (*→*)→*→* *→* *`[Resource, IO, A] =
          io.dot(
            `Value.Def.0: *→*→* (*→* *) ((*→*)→*→* *→* *)`(
              0,
              `DefSym.0`("toResource"),
              io.tpe ==> `Type.App: (*→*)→*→* *→* *`(0, Resource, IO, io.tpe.targ1),
              None
            )
          )