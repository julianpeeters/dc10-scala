package dc10.scala.predef.calculus.variables

import cats.data.StateT
// import cats.syntax.all.toTraverseOps
import dc10.scala.{*, given}
// import dc10.scala.predef.calculus.Functions.function1

trait Type[F[_]]:
  
  @scala.annotation.targetName("*")
  def TYPE[T](nme: String): F[`Type.Var: *`[T]]
  @scala.annotation.targetName("*→*")
  def TYPE[G[_], A](nme: String, tparam: F[`Type.Var: *`[A]]): F[`Type.Var: *→*`[G]]
  @scala.annotation.targetName("(*→*)→*")
  def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type.Var: *→*`[H]]): F[`Type.Var: (*→*)→*`[G]]
  @scala.annotation.targetName("(*→*)→*→*")
  def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type.Var: *→*`[H]], targA: F[`Type.Var: *`[A]]): F[`Type.Var: (*→*)→*→*`[G]]

  
object Type:

  def A[A]: `Type.Var: *`[A] = `Type.Var: *`[A](0, "A", None)
  
  // extension (nme: String)
  //   def ::[A](tpe: `Type.Var: *`[A]): `Value.Var.Unbound.Data`[A] = `Value.Var.Unbound.Data`[A](0, nme, tpe)

  trait Mixins extends Type[StateT[ErrorF, Γ, _]]:
  

    @scala.annotation.targetName("*")
    def TYPE[T](nme: String): StateT[ErrorF, Γ, `Type.Var: *`[T]] =
      for
        t <- StateT.pure[ErrorF, Γ, `Type.Var: *`[T]](`Type.Var: *`(0, nme, None))
        d <- StateT.pure(Statement.`type`[T](t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("*→*")
    def TYPE[G[_], A](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type.Var: *`[A]]
    ): StateT[ErrorF, Γ, `Type.Var: *→*`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var: *→*`[G](0, nme, None, () => Nil))
        d <- StateT.pure(Statement.`type`.`[_]`(a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("(*→*)→*")
    def TYPE[G[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type.Var: *→*`[H]]
    ): StateT[ErrorF, Γ, `Type.Var: (*→*)→*`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var: (*→*)→*`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_]]`(a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("(*→*)→*→*")
    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, Γ, `Type.Var: *→*`[H]],
      targA: StateT[ErrorF, Γ, `Type.Var: *`[A]]
    ): StateT[ErrorF, Γ, `Type.Var: (*→*)→*→*`[G]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(`Type.Var: (*→*)→*→*`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_], _]`[G, H, A](f, a, t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

    // def VAL[`T.*`[t] <: `Type.Expr: *`[t], T](
    //   nme: String,
    //   tpe: StateT[ErrorF, Γ, `T.*`[T]]
    // ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
    //   for
    //     t <- StateT.liftF(tpe.runEmptyA)
    //     v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
    //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.`val`(v))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield v