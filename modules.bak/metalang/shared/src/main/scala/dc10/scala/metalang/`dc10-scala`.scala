package dc10.scala.metalang

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions

trait `dc10-scala`[F[_]]:
  def ERRORF: F[`Type: x→x`[ErrorF]]
  def LIBDEP: F[`Type: x`[Dep]]
  def STATEMENT: F[`Type: x`[Statement]]
  def `TYPEEXPR`[G[_], A](targ: F[`Type: x`[A]]): F[`Type: x`[G[A]]]
  @scala.annotation.targetName("_[_[_], _]")
  def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](targ: F[`Type: (x→x)→x→x`[H]]): F[`Type: x`[G[H]]]
  @scala.annotation.targetName("Type[_[_], _]")
  def TYPEEXPR[T[_[_], _]](arg: F[`Value.Var.Unbound.Data`[`Type.Var: (x→x)→x→x`[T]]]): F[`Value: x`[`Type: (x→x)→x→x`[T]]]
  // def VALUEEXPR[T](arg: F[`Value: x`[T]]): F[`Value.App.1: x`[`Value.Var.Unbound.Data`[T], `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]]]
  def `TYPEEXPR[_]`[G[_[_]], H[_]](targ: F[`Type: x→x`[H]]): F[`Type: x`[G[H]]]
  def `Type.Var: (x→x)→x→x`[G[_[_], _]](nme: String): F[`Value: x`[`Type.Var: (x→x)→x→x`[G]]]
  @scala.annotation.targetName("Value[_[_]]")
  def VALUEEXPR[G[_], A](targ: F[`Type: x`[A]]): F[`Type: x`[G[A]]]
  // extension (ctx: `Value: x`[Γ])
  //   def DEP(d: F[`Value: x`[Dep]]): F[`Value: x`[ErrorF[Γ]]]

object `dc10-scala`:

  val lib: Dep = Dep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends `dc10-scala`[StateT[ErrorF, Γ, _]]:

    def ERRORF: StateT[ErrorF, Γ, `Type: x→x`[ErrorF]] =
      StateT.pure(`Type.Var: x→x`[ErrorF](0, "dc10.scala.ErrorF", scala.None, () => Nil))

    def LIBDEP: StateT[ErrorF, Γ, `Type: x`[Dep]] =
      StateT.pure(`Type.Var: x`[Dep](0, "dc10.scala.Dep", scala.None))

    def Dep(
      org: StateT[ErrorF, Γ, `Value: x`[String]],
      nme: StateT[ErrorF, Γ, `Value: x`[String]],
      ver: StateT[ErrorF, Γ, `Value: x`[String]]
    ): StateT[ErrorF, Γ, `Value: x`[Dep]] =
      for
        o <- org
        n <- nme
        v <- ver
        t <- LIBDEP
        f <- StateT.pure[ErrorF, Γ, (`Type: x`[String], `Type: x`[String], `Type: x`[String])]((o.tpe, n.tpe, v.tpe)) ==> t
      yield `Value.App.3: x`(0, `Value.Var.Unbound.Data`[(String, String, String) => Dep](0, "dc10.scala.Dep", f), o, n, v, t)

    def STATEMENT: StateT[ErrorF, Γ, `Type: x`[Statement]] =
      StateT.pure(`Type.Var: x`[Statement](0, "dc10.scala.Statement", scala.None))

    def `TYPEEXPR`[G[_], A](
      targ: StateT[ErrorF, Γ, `Type: x`[A]]
    ): StateT[ErrorF, Γ, `Type: x`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var: x→x`(0, "dc10.scala.`Type: x`", scala.None, () => Nil),
          a
        )

    @scala.annotation.targetName("_[_[_], _]")
    def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](
      targ: StateT[ErrorF, Γ, `Type: (x→x)→x→x`[H]]
    ): StateT[ErrorF, Γ, `Type: x`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_[_], _]]`(
          0,
          `Type.Var: ((x→x)→x→x)→x`(0, "dc10.scala.`Type: (x→x)→x→x`", scala.None),
          a
        )

    @scala.annotation.targetName("Type[_[_], _]")
    def TYPEEXPR[T[_[_], _]](arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[`Type.Var: (x→x)→x→x`[T]]]): StateT[ErrorF, Γ, `Value: x`[`Type: (x→x)→x→x`[T]]] =
      for
        a <- arg
        t <- StateT.pure(`Value.Var.Unbound.Data`[`Type: (x→x)→x→x`[T]](0, "", `Type.Var: x`(0, "dc10.scala.`Type.Var: (x→x)→x→x`", scala.None)))
        f <- StateT.pure[ErrorF, Γ, `Value.Var.Unbound.Data`[`Type.Var: (x→x)→x→x`[T]]](a) ==> (x => StateT.pure(t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield 
        `Value.App.1: x`(
          0,
          `Value.Var.Unbound.Data`[`Type.Var: (x→x)→x→x`[T] => `Type: (x→x)→x→x`[T]](0, "dc10.scala.`Type.Var: (x→x)→x→x`", f.tpe),
          a,
          t.tpe
        )
      
    // def `VALUEEXPR`[T](
    //   arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]]
    // ): StateT[ErrorF, Γ, `Value.App.1: x`[`Value.Var.Unbound.Data`[T], `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]]] =
    //   for
    //     a <- arg
    //     t <- StateT.pure(`Type.Var: x`[`Value: x`[T]](0, "dc10.scala.`Type: (x→x)→x→x`", scala.None))
    //     f <- arg ==> (x => StateT.pure[ErrorF, Γ, `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]](`Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]](0, "SDSD", t)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
    //   yield 
    //     `Value.App.1: x`(0, f, a, t)

    def `TYPEEXPR[_]`[G[_[_]], H[_]](
      targ: StateT[ErrorF, Γ, `Type: x→x`[H]]
    ): StateT[ErrorF, Γ, `Type: x`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_]]`(
          0,
          `Type.Var: (x→x)→x`(0, "dc10.scala.`Type: x→x`", scala.None),
          a
        )

    def `Type.Var: (x→x)→x→x`[G[_[_], _]](nme: String): StateT[ErrorF, Γ, `Value: x`[`Type.Var: (x→x)→x→x`[G]]] =
      StateT.pure(
        `Value.Var.Unbound.Data`(
          0,
          "dc10.scala.`Type.Var: (x→x)→x→x`",
          `Type.Var: x`(0, s"$nme**", scala.None)
        )
      )

    @scala.annotation.targetName("Value[_[_]]")
    def VALUEEXPR[G[_], A](
      targ: StateT[ErrorF, Γ, `Type: x`[A]]
    ): StateT[ErrorF, Γ, `Type: x`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var: x→x`(0, "dc10.scala.`Value: x`", scala.None, () => Nil),
          a
        )

    // extension (ctx: `Value: x`[Γ])
    //   def DEP(d: StateT[ErrorF, Γ, `Value: x`[Dep]]): StateT[ErrorF, Γ, `Value: x`[ErrorF[Γ]]] =
    //     for
    //       f <- VAL("dep", ctx.tpe ==> ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT))))
    //       t <- ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT)))
    //       a <- d
    //     yield `Value.AppDot.1: x`(0, f, ctx, a, t)