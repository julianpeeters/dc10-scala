package dc10.scala.predef.calculus.variables

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{*, given}
import dc10.scala.predef.calculus.Functions.function1

trait Def[F[_]]:

  @scala.annotation.targetName("0")
  def DEF[T, `T.*`[t] <: `Type.Expr: *`[t]](nme: String, tpe: F[`T.*`[T]]): F[`Value.Var.Unbound.Data`[T]]
  // def DEF[T](nme: String, tpe: F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]



  @scala.annotation.targetName("0*")
  def DEF[A, T](nme: String, tparam: F[`Type.Var: *`[A]], tpe: `Type.Expr: *`[A] => F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]
  @scala.annotation.targetName("0*→*→*")
  def DEF[A, B, T](nme: String, tparam1: F[`Type.Var: *`[A]], tparam2: F[`Type.Var: *`[B]], tpe: (`Type.Expr: *`[A], `Type.Expr: *`[B]) => F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]
  @scala.annotation.targetName("0*→*")
  def DEF[G[_], T](nme: String, tparam: F[`Type: *→*`[G]], tpe: `Type: *→*`[G] => F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]
  @scala.annotation.targetName("0(*→*)→*→*")
  // def DEF[G[_], A, T, `F.*→*`[t[_]] <: `Type: *→*`[t], `A.*`[t] <: `Type.Expr: *`[t], `T.*`[t] <: `Type.Expr: *`[t]](nme: String, tparamf: F[`F.*→*`[G]], tparama: F[`A.*`[A]], tpe: (`F.*→*`[G], `A.*`[A]) => F[`T.*`[T]]): F[`Value.Var.Unbound.Data`[T]]
  def DEF[G[_], A, T](nme: String, tparamf: F[`Type: *→*`[G]], tparama: F[`Type.Expr: *`[A]], tpe: (`Type: *→*`[G], `Type.Expr: *`[A]) => F[`Type.Expr: *`[T]]): F[`Value.Var.Unbound.Data`[T]]
  
  // @scala.annotation.targetName("1")
  // // def DEF[G[_], A, T, `T.*`[t] <: `Type.Expr: *`[t], `V.*`[t] <: `Value.Expr: *`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type.Expr: *`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
  // def DEF[A, T, `T.*`[t] <: `Type.Expr: *`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`T.*`[T]]): F[`Value.Var.Unbound.Data`[A => T]]


  @scala.annotation.targetName("1 App")
  // def DEF[G[_], A, T, `T.*`[t] <: `Type.Expr: *`[t], `V.*`[t] <: `Value.Expr: *`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type.Expr: *`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
  def DEF[G[_], A](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type.App[_]`[G, A]]): F[`Value.Var.Unbound.Data`[A => G[A]]]

  @scala.annotation.targetName("1*")
  def DEF[A, B, T](nme: String, tparam: F[`Type.Expr: *`[A]], arg: F[`Value.Expr: *`[B]], tpe: `Type.Expr: *`[A] => F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]
  
  @scala.annotation.targetName("1(*→*)→*")
  def DEF[G[_], A, B, T](nme: String, tparamf: F[`Type: *→*`[G]], tparama: F[`Type.Expr: *`[A]], arg: F[`Value.Expr: *`[B]], tpe: (`Type: *→*`[G], `Type.Expr: *`[A]) => F[`Type.Expr: *`[T]]): F[`Value.Expr: *`[T]]

object Def:

  
  // extension (nme: String)
  //   def ::[A](tpe: `Type.Var: *`[A]): `Value.Var.Unbound.Data`[A] = `Value.Var.Unbound.Data`[A](0, nme, tpe)

  trait Mixins extends Def[StateT[ErrorF, Γ, _]]:

    @scala.annotation.targetName("0")
    def DEF[T, `T.*`[t] <: `Type.Expr: *`[t]](
    // def DEF[T](
      nme: String, 
      tpe: StateT[ErrorF, Γ, `T.*`[T]]
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
      tparam: StateT[ErrorF, Γ, `Type.Var: *`[A]],
      tpe: `Type.Expr: *`[A] => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`.`[_]`(a, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*→*→*")
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, Γ, `Type.Var: *`[A]],
      tparam2: StateT[ErrorF, Γ, `Type.Var: *`[B]],
      tpe: (`Type.Expr: *`[A], `Type.Expr: *`[B]) => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*→*")
    def DEF[G[_], T](
      nme: String,
      tparam: StateT[ErrorF, Γ, `Type: *→*`[G]],
      tpe: `Type: *→*`[G] => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[T]] =
      for
        f <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f).runEmpty)
        v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, t))
        d <- StateT.pure[ErrorF, Γ, Statement](Statement.`def`.`0`.`[_[_]]`(f, None, v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield v
      
    @scala.annotation.targetName("0(*→*)→*→*")
    // def DEF[G[_], A, T, `F.*→*`[t[_]] <: `Type: *→*`[t], `A.*`[t] <: `Type.Expr: *`[t], `T.*`[t] <: `Type.Expr: *`[t]](
    def DEF[G[_], A, T](
      nme: String,
      tparamf: StateT[ErrorF, Γ, `Type: *→*`[G]],
      tparama: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
      tpe: (`Type: *→*`[G], `Type.Expr: *`[A]) => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
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
    // def DEF[G[_], A, T, `T.*`[t] <: `Type.Expr: *`[t], `V.*`[t] <: `Value.Expr: *`[t]](
    def DEF[A, T, `T.*`[t] <: `Type.Expr: *`[t]](
      nme: String,
      arg: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A]], 
      tpe: StateT[ErrorF, Γ, `T.*`[T]]
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
  // def DEF[G[_], A, T, `T.*`[t] <: `Type.Expr: *`[t], `V.*`[t] <: `Value.Expr: *`[t]](nme: String, arg: F[`Value.Var.Unbound.Data`[A]], tpe: F[`Type.Expr: *`[T]]): F[`Value.Var.Unbound.Data`[A => T]]
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
      tparam: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
      arg: StateT[ErrorF, Γ, `Value.Expr: *`[B]],
      tpe: `Type.Expr: *`[A] => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[T]] =
      ???

    @scala.annotation.targetName("1(*→*)→*")
    def DEF[G[_], A, B, T](
      nme: String,
      tparamf: StateT[ErrorF, Γ, `Type: *→*`[G]],
      tparama: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
      arg: StateT[ErrorF, Γ, `Value.Expr: *`[B]],
      tpe: (`Type: *→*`[G], `Type.Expr: *`[A]) => StateT[ErrorF, Γ, `Type.Expr: *`[T]]
    ): StateT[ErrorF, Γ, `Value.Expr: *`[T]] =
      ???
  
