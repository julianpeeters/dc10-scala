package dc10.scala.predef.calculus

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.internal.implementation.implement

object assignment:

  extension [T] (lhs: StateT[ErrorF, Γ, `Type.Var: *`[T]])
    def :=(
      rhs: `Type.Expr: *`[T]
    ): StateT[ErrorF, Γ, `Type.Var: *`[T]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        t <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`TypeDef: *`(t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t
    
  extension [T[_, _], A, B] (lhs: StateT[ErrorF, Γ, `Value.Def.1: *→*→* * *`[A, B]])
    @scala.annotation.targetName("assignDef1")
    def :=(
      rhs: `Value.Expr: *`[A] => `Value.Expr: *`[B]
    ): StateT[ErrorF, Γ, `Value.Def.1: *→*→* * *`[A, B]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs(l.sym.arg1)))
        d <- StateT.pure(`DefDef: *→*→* * *`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

  extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Val: *→*→* * *`[Function1, A, B]])
    @scala.annotation.targetName("assignVal1")
    def :=(
      rhs: `Value.Lam.1: *→*→* * *`[A, B]
    ): StateT[ErrorF, Γ, `Value.Val: *→*→* * *`[Function1, A, B]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`ValDef: *→*→* * *`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v


  extension [T] (lhs: StateT[ErrorF, Γ, `Value.Val: *`[T]])
    @scala.annotation.targetName("assignVal2")
    def :=(
      rhs: `Value.Expr: *`[T]
    ): StateT[ErrorF, Γ, `Value.Val: *`[T]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`ValDef: *`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

  extension [T[_], A] (lhs: StateT[ErrorF, Γ, `Value.Val: *→* *`[T, A]])
    def :=(
      rhs: `Value.Expr: *→* *`[T, A]
    ): StateT[ErrorF, Γ, `Value.Val: *→* *`[T, A]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`ValDef: *→* *`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

  extension [T[_], G[_[_], _], H[_], A] (lhs: StateT[ErrorF, Γ, `Value.Val: *→* ((*→*)→*→* *→* *)`[T, G, H, A]])
    def :=(
      rhs: `Value.Expr: *→* ((*→*)→*→* *→* *)`[T, G, H, A]
    ): StateT[ErrorF, Γ, `Value.Val: *→* ((*→*)→*→* *→* *)`[T, G, H, A]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`ValDef: *→* ((*→*)→*→* *→* *)`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

  extension [T[_[_], _], F[_], A] (lhs: StateT[ErrorF, Γ, `Value.Val: (*→*)→*→* *→* *`[T, F, A]])
    def :=(
      rhs: `Value.Expr: (*→*)→*→* *→* *`[T, F, A]
    ): StateT[ErrorF, Γ, `Value.Val: (*→*)→*→* *→* *`[T, F, A]] =
      for
        l <- StateT.liftF(lhs.runEmptyA)
        v <- StateT.pure(l.implement(rhs))
        d <- StateT.pure(`ValDef: (*→*)→*→* *→* *`(v))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

//   extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Comp`[A, B]])
//     def :=(
//       rhs: `Value.Var.Bound.Comp`[A, B]
//     ): StateT[ErrorF, Γ, `Value.Var.Bound.Comp`[A, B]] =
//       for
//         l <- StateT.liftF(lhs.runEmptyA)
//         // s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
//         // (d, v) <- StateT.liftF(s.implement(rhs))
//         v <- StateT.pure(l.implement(rhs))
//         d <- StateT.pure(v.define)
//         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
//       yield ???
  

//   extension [T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]])
//     @scala.annotation.targetName("assign value")
//     def :=(
//       rhs: `Value.Expr: *`[T]
//     ): StateT[ErrorF, Γ, `Value.Var.Bound.Data`[T]] =
//       for
//         l <- StateT.liftF(lhs.runEmptyA)
//         // s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
//         // (d, v) <- StateT.liftF(s.implement(rhs))
//         v <- StateT.pure(l.implement(rhs))
//         d <- StateT.pure(v.define)
//         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
//       yield v






  // extension [T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]])
  // @scala.annotation.targetName("assign comp")
  // def ==(
  //   rhs: `Value.Var`[T]
  // ): StateT[ErrorF, Γ, `Value.Var.Bound.Data`[T]] =
  //   for
  //     ctx <- StateT.liftF(lhs.runEmptyS)
  //     s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
  //     (d, v) <- StateT.liftF(s.implement(rhs))
  //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //   yield v



