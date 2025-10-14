package dc10.scala.predef.calculus.variables

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{*, given}
import dc10.scala.predef.calculus.Functions.function1

trait Def[F[_]]:

  @scala.annotation.targetName("0")
  def DEF[T, `T.x`[t] <: `Type: x`[t]](nme: String, tpe: F[`T.x`[T]]): F[`Value.Var.Unbound.Data`[T]]
  // def DEF[T](nme: String, tpe: F[`Type: x`[T]]): F[`Value: x`[T]]



  @scala.annotation.targetName("0*")
  def DEF[A, T](nme: String, tparam: F[`Type.Var: x`[A]], tpe: `Type: x`[A] => F[`Type: x`[T]]): F[`Value: x`[T]]
  @scala.annotation.targetName("0x_x_x")
  def DEF[A, B, T](nme: String, tparam1: F[`Type.Var: x`[A]], tparam2: F[`Type.Var: x`[B]], tpe: (`Type: x`[A], `Type: x`[B]) => F[`Type: x`[T]]): F[`Value: x`[T]]
  @scala.annotation.targetName("0x_x")
  def DEF[G[_], T](nme: String, tparam: F[`Type: x_x`[G]], tpe: `Type: x_x`[G] => F[`Type: x`[T]]): F[`Value: x`[T]]
  @scala.annotation.targetName("0lx_xl_x_x")
  // def DEF[G[_], A, T, `F.x_x`[t[_]] <: `Type: x_x`[t], `A.x`[t] <: `Type: x`[t], `T.x`[t] <: `Type: x`[t]](nme: String, tparamf: F[`F.x_x`[G]], tparama: F[`A.x`[A]], tpe: (`F.x_x`[G], `A.x`[A]) => F[`T.x`[T]]): F[`Value.Var.Unbound.Data`[T]]
  def DEF[G[_], A, T](nme: String, tparamf: F[`Type: x_x`[G]], tparama: F[`Type: x`[A]], tpe: (`Type: x_x`[G], `Type: x`[A]) => F[`Type: x`[T]]): F[`Value.Var.Unbound.Data`[T]]
  
  // @scala.annotation.targetName("1")
  // // def DEF[G[_], A, T, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type: x`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
  // def DEF[A, T, `T.x`[t] <: `Type: x`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`T.x`[T]]): F[`Value.Var.Unbound.Data`[A => T]]


  @scala.annotation.targetName("1 App")
  // def DEF[G[_], A, T, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type: x`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
  def DEF[G[_], A](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type.App[_]`[G, A]]): F[`Value.Var.Unbound.Data`[A => G[A]]]

  @scala.annotation.targetName("1*")
  def DEF[A, B, T](nme: String, tparam: F[`Type: x`[A]], arg: F[`Value: x`[B]], tpe: `Type: x`[A] => F[`Type: x`[T]]): F[`Value: x`[T]]
  
  @scala.annotation.targetName("1lx_xl_x")
  def DEF[G[_], A, B, T](nme: String, tparamf: F[`Type: x_x`[G]], tparama: F[`Type: x`[A]], arg: F[`Value: x`[B]], tpe: (`Type: x_x`[G], `Type: x`[A]) => F[`Type: x`[T]]): F[`Value: x`[T]]

object Def:

  
  // extension (nme: String)
  //   def ::[A](tpe: `Type.Var: x`[A]): `Value.Var.Unbound.Data`[A] = `Value.Var.Unbound.Data`[A](0, nme, tpe)

  trait Mixins extends Def[StateT[ErrorF, Γ, _]]:

    @scala.annotation.targetName("0")
    def DEF[T, `T.x`[t] <: `Type: x`[t]](
    // def DEF[T](
      nme: String, 
      tpe: StateT[ErrorF, Γ, `T.x`[T]]
    ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*")
    def DEF[A, T](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type.Var: x`[A]],
      tpe: `Type: x`[A] => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`.`[_]`(a, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0x_x_x")
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, Γ, `Type.Var: x`[A]],
      tparam2: StateT[ErrorF, Γ, `Type.Var: x`[B]],
      tpe: (`Type: x`[A], `Type: x`[B]) => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0x_x")
    def DEF[G[_], T](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type: x_x`[G]],
      tpe: `Type: x_x`[G] => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        f <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`.`[_[_]]`(f, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v
      
    @scala.annotation.targetName("0lx_xl_x_x")
    // def DEF[G[_], A, T, `F.x_x`[t[_]] <: `Type: x_x`[t], `A.x`[t] <: `Type: x`[t], `T.x`[t] <: `Type: x`[t]](
    def DEF[G[_], A, T](
      nme: String,
      tparamf: StateT[ErrorF, Γ, `Type: x_x`[G]],
      tparama: StateT[ErrorF, Γ, `Type: x`[A]],
      tpe: (`Type: x_x`[G], `Type: x`[A]) => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
      for
        f <- StateT.liftF(tparamf.runEmptyA)
        a <- StateT.liftF(tparama.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f, a).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`.`[_[_], _]`(f, a, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("1")
    // def DEF[G[_], A, T, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](
    def DEF[A, T, `T.x`[t] <: `Type: x`[t]](
      nme: String,
      arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A]], 
      tpe: StateT[ErrorF, Γ, `T.x`[T]]
    ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure(function1(a.tpe, r))
        v <- StateT.pure(`Value.Var.Unbound.Data`[A => T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`1`(a, r, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v


    @scala.annotation.targetName("1 App")
  // def DEF[G[_], A, T, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type: x`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
    def DEF[G[_], A](
      nme: String,
      arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A]],
      tpe: StateT[ErrorF, Γ, `Type.App[_]`[G, A]]
    ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A => G[A]]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure(function1(a.tpe, r))
        v <- StateT.pure(`Value.Var.Unbound.Data`[A => G[A]](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`1`(a, r, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("1*")
    def DEF[A, B, T](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type: x`[A]],
      arg: StateT[ErrorF, Γ, `Value: x`[B]],
      tpe: `Type: x`[A] => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      ???

    @scala.annotation.targetName("1lx_xl_x")
    def DEF[G[_], A, B, T](
      nme: String,
      tparamf: StateT[ErrorF, Γ, `Type: x_x`[G]],
      tparama: StateT[ErrorF, Γ, `Type: x`[A]],
      arg: StateT[ErrorF, Γ, `Value: x`[B]],
      tpe: (`Type: x_x`[G], `Type: x`[A]) => StateT[ErrorF, Γ, `Type: x`[T]]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      ???
  
