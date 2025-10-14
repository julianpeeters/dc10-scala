package dc10.scala.predef.calculus.variables

import cats.data.StateT
// import cats.syntax.all.toTraverseOps
import dc10.scala.{*, given}
// import dc10.scala.predef.calculus.Functions.function1

trait Type[F[_]]:
  
  @scala.annotation.targetName("*")
  def TYPE[T](nme: String): F[`Type.Var: x`[T]]
  @scala.annotation.targetName("x_x")
  def TYPE[G[_], A](nme: String, tparam: F[`Type.Var: x`[A]]): F[`Type.Var: x_x`[G]]
  @scala.annotation.targetName("lx_xl_x")
  def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type.Var: x_x`[H]]): F[`Type.Var: lx_xl_x`[G]]
  @scala.annotation.targetName("lx_xl_x_x")
  def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type.Var: x_x`[H]], targA: F[`Type.Var: x`[A]]): F[`Type.Var: lx_xl_x_x`[G]]

  
object Type:

  def A[A]: `Type.Var: x`[A] = `Type.Var: x`[A](0, "A", None)
  
  // extension (nme: String)
  //   def ::[A](tpe: `Type.Var: x`[A]): `Value.Var.Unbound.Data`[A] = `Value.Var.Unbound.Data`[A](0, nme, tpe)

  trait Mixins extends Type[StateT[ErrorF, Γ, _]]:
  

    @scala.annotation.targetName("*")
    def TYPE[T](nme: String): StateT[ErrorF, Γ, `Type.Var: x`[T]] =
      for
        t <- StateT.pure[ErrorF, Γ, `Type.Var: x`[T]](`Type.Var: x`(0, nme, None))
        d <- StateT.pure(Statement.`type`[T](t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("x_x")
    def TYPE[G[_], A](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type.Var: x`[A]]
    ): StateT[ErrorF, Γ, `Type.Var: x_x`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var: x_x`[G](0, nme, None, () => Nil))
        d <- StateT.pure(Statement.`type`.`[_]`(a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("lx_xl_x")
    def TYPE[G[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type.Var: x_x`[H]]
    ): StateT[ErrorF, Γ, `Type.Var: lx_xl_x`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var: lx_xl_x`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_]]`(a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("lx_xl_x_x")
    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, Γ, `Type.Var: x_x`[H]],
      targA: StateT[ErrorF, Γ, `Type.Var: x`[A]]
    ): StateT[ErrorF, Γ, `Type.Var: lx_xl_x_x`[G]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(`Type.Var: lx_xl_x_x`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_], _]`[G, H, A](f, a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    // def VAL[`T.x`[t] <: `Type: x`[t], T](
    //   nme: String,
    //   tpe: StateT[ErrorF, Γ, `T.x`[T]]
    // ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
    //   for
    //     t <- StateT.liftF(tpe.runEmptyA)
    //     v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
    //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.`val`(v))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield v