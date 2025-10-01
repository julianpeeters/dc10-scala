package dc10.scala.internal

import dc10.scala.*

object implementation:

  extension [T] (lhs: `Type.Var: *`[T])
    def implement(rhs: `Type.Expr: *`[T]): `Type.Var: *`[T] =
      `Type.Var: *`[T](lhs.lvl, lhs.sym, Some(rhs))

  extension [T[_, _], A, B] (lhs: `Value.Val: *→*→* * *`[T, A, B])
    // def implement(rhs: `Value.Lam.1: *→*→* * *`[A, B]): `Value.Val: *→*→* * *`[T, A, B] =
    //   `Value.Val: *→*→* * *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))
    // def implement(rhs: `Value.Val: *→*→* * *`[T, A, B]): `Value.Val: *→*→* * *`[T, A, B] =
    //   `Value.Val: *→*→* * *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))
    def implement(rhs: `Value.Expr: *→*→* * *`[T, A, B]): `Value.Val: *→*→* * *`[T, A, B] =
      `Value.Val: *→*→* * *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T] (lhs: `Value.Val: *`[T])
    def implement(rhs: `Value.Expr: *`[T]): `Value.Val: *`[T] =
      `Value.Val: *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T[_], A] (lhs: `Value.Val: *→* *`[T, A])
    def implement(rhs: `Value.Expr: *→* *`[T, A]): `Value.Val: *→* *`[T, A] =
      `Value.Val: *→* *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T[_[_], _], F[_], A] (lhs: `Value.Val: (*→*)→*→* *→* *`[T, F, A])
    def implement(rhs: `Value.Expr: (*→*)→*→* *→* *`[T, F, A]): `Value.Val: (*→*)→*→* *→* *`[T, F, A] =
      `Value.Val: (*→*)→*→* *→* *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))


  // extension [A, B] (lhs: `Value.Var.Unbound.Comp.Def.0`[A, B])
  //   def implement(rhs: `Value.Expr: *`[A => B]): `Value.Var.Bound.Comp.Def.0`[A, B] =
  //     `Value.Var.Bound.Comp.Def.0`(lhs.lvl, lhs.sym, lhs.tpe, lhs.ret, rhs)

  extension [T[_, _], A, B] (lhs: `Value.Def.1: *→*→* * *`[A, B])
    def implement(rhs: `Value.Expr: *`[B]): `Value.Def.1: *→*→* * *`[A, B] =
      `Value.Def.1: *→*→* * *`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))