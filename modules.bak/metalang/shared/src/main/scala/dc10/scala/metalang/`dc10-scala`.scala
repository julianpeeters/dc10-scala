package dc10.scala.metalang

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions

trait `dc10-scala`[F[_]]:
  def ERRORF: F[`Type: *→*`[ErrorF]]
  def LIBDEP: F[`Type.Expr: *`[Dep]]
  def STATEMENT: F[`Type.Expr: *`[Statement]]
  def `TYPEEXPR`[G[_], A](targ: F[`Type.Expr: *`[A]]): F[`Type.Expr: *`[G[A]]]
  @scala.annotation.targetName("_[_[_], _]")
  def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](targ: F[`Type: (*→*)→*→*`[H]]): F[`Type.Expr: *`[G[H]]]
  @scala.annotation.targetName("Type[_[_], _]")
  def TYPEEXPR[T[_[_], _]](arg: F[`Value.Var.Unbound.Data`[`Type.Var: (*→*)→*→*`[T]]]): F[`Value.Expr: *`[`Type: (*→*)→*→*`[T]]]
  // def VALUEEXPR[T](arg: F[`Value.Expr: *`[T]]): F[`Value.App.1: *`[`Value.Var.Unbound.Data`[T], `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]]]
  def `TYPEEXPR[_]`[G[_[_]], H[_]](targ: F[`Type: *→*`[H]]): F[`Type.Expr: *`[G[H]]]
  def `Type.Var: (*→*)→*→*`[G[_[_], _]](nme: String): F[`Value.Expr: *`[`Type.Var: (*→*)→*→*`[G]]]
  @scala.annotation.targetName("Value[_[_]]")
  def VALUEEXPR[G[_], A](targ: F[`Type.Expr: *`[A]]): F[`Type.Expr: *`[G[A]]]
  // extension (ctx: `Value.Expr: *`[Γ])
  //   def DEP(d: F[`Value.Expr: *`[Dep]]): F[`Value.Expr: *`[ErrorF[Γ]]]

object `dc10-scala`:

  val lib: Dep = Dep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends `dc10-scala`[StateT[ErrorF, Γ, _]]:

    def ERRORF: StateT[ErrorF, Γ, `Type: *→*`[ErrorF]] =
      StateT.pure(`Type.Var: *→*`[ErrorF](0, "dc10.scala.ErrorF", scala.None, () => Nil))

    def LIBDEP: StateT[ErrorF, Γ, `Type.Expr: *`[Dep]] =
      StateT.pure(`Type.Var: *`[Dep](0, "dc10.scala.Dep", scala.None))

    def Dep(
      org: StateT[ErrorF, Γ, `Value.Expr: *`[String]],
      nme: StateT[ErrorF, Γ, `Value.Expr: *`[String]],
      ver: StateT[ErrorF, Γ, `Value.Expr: *`[String]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[Dep]] =
      for
        o <- org
        n <- nme
        v <- ver
        t <- LIBDEP
        f <- StateT.pure[ErrorF, Γ, (`Type.Expr: *`[String], `Type.Expr: *`[String], `Type.Expr: *`[String])]((o.tpe, n.tpe, v.tpe)) ==> t
      yield `Value.App.3: *`(0, `Value.Var.Unbound.Data`[(String, String, String) => Dep](0, "dc10.scala.Dep", f), o, n, v, t)

    def STATEMENT: StateT[ErrorF, Γ, `Type.Expr: *`[Statement]] =
      StateT.pure(`Type.Var: *`[Statement](0, "dc10.scala.Statement", scala.None))

    def `TYPEEXPR`[G[_], A](
      targ: StateT[ErrorF, Γ, `Type.Expr: *`[A]]
    ): StateT[ErrorF, Γ, `Type.Expr: *`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var: *→*`(0, "dc10.scala.`Type.Expr: *`", scala.None, () => Nil),
          a
        )

    @scala.annotation.targetName("_[_[_], _]")
    def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](
      targ: StateT[ErrorF, Γ, `Type: (*→*)→*→*`[H]]
    ): StateT[ErrorF, Γ, `Type.Expr: *`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_[_], _]]`(
          0,
          `Type.Var: ((*→*)→*→*)→*`(0, "dc10.scala.`Type: (*→*)→*→*`", scala.None),
          a
        )

    @scala.annotation.targetName("Type[_[_], _]")
    def TYPEEXPR[T[_[_], _]](arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[`Type.Var: (*→*)→*→*`[T]]]): StateT[ErrorF, Γ, `Value.Expr: *`[`Type: (*→*)→*→*`[T]]] =
      for
        a <- arg
        t <- StateT.pure(`Value.Var.Unbound.Data`[`Type: (*→*)→*→*`[T]](0, "", `Type.Var: *`(0, "dc10.scala.`Type.Var: (*→*)→*→*`", scala.None)))
        f <- StateT.pure[ErrorF, Γ, `Value.Var.Unbound.Data`[`Type.Var: (*→*)→*→*`[T]]](a) ==> (x => StateT.pure(t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield 
        `Value.App.1: *`(
          0,
          `Value.Var.Unbound.Data`[`Type.Var: (*→*)→*→*`[T] => `Type: (*→*)→*→*`[T]](0, "dc10.scala.`Type.Var: (*→*)→*→*`", f.tpe),
          a,
          t.tpe
        )
      
    // def `VALUEEXPR`[T](
    //   arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]]
    // ): StateT[ErrorF, Γ, `Value.App.1: *`[`Value.Var.Unbound.Data`[T], `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]]] =
    //   for
    //     a <- arg
    //     t <- StateT.pure(`Type.Var: *`[`Value.Expr: *`[T]](0, "dc10.scala.`Type: (*→*)→*→*`", scala.None))
    //     f <- arg ==> (x => StateT.pure[ErrorF, Γ, `Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]]](`Value.Var.Unbound.Data`[`Value.Var.Unbound.Data`[T]](0, "SDSD", t)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
    //   yield 
    //     `Value.App.1: *`(0, f, a, t)

    def `TYPEEXPR[_]`[G[_[_]], H[_]](
      targ: StateT[ErrorF, Γ, `Type: *→*`[H]]
    ): StateT[ErrorF, Γ, `Type.Expr: *`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_]]`(
          0,
          `Type.Var: (*→*)→*`(0, "dc10.scala.`Type: *→*`", scala.None),
          a
        )

    def `Type.Var: (*→*)→*→*`[G[_[_], _]](nme: String): StateT[ErrorF, Γ, `Value.Expr: *`[`Type.Var: (*→*)→*→*`[G]]] =
      StateT.pure(
        `Value.Var.Unbound.Data`(
          0,
          "dc10.scala.`Type.Var: (*→*)→*→*`",
          `Type.Var: *`(0, s"$nme**", scala.None)
        )
      )

    @scala.annotation.targetName("Value[_[_]]")
    def VALUEEXPR[G[_], A](
      targ: StateT[ErrorF, Γ, `Type.Expr: *`[A]]
    ): StateT[ErrorF, Γ, `Type.Expr: *`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var: *→*`(0, "dc10.scala.`Value.Expr: *`", scala.None, () => Nil),
          a
        )

    // extension (ctx: `Value.Expr: *`[Γ])
    //   def DEP(d: StateT[ErrorF, Γ, `Value.Expr: *`[Dep]]): StateT[ErrorF, Γ, `Value.Expr: *`[ErrorF[Γ]]] =
    //     for
    //       f <- VAL("dep", ctx.tpe ==> ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT))))
    //       t <- ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT)))
    //       a <- d
    //     yield `Value.AppDot.1: *`(0, f, ctx, a, t)