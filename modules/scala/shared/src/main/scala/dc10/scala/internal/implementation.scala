package dc10.scala.internal

import dc10.scala.*

object implementation:

  extension [T] (lhs: `Type.Var: x`[T])
    def implement(rhs: `Type: x`[T]): `Type.Var: x`[T] =
      `Type.Var: x`[T](lhs.lvl, lhs.sym, Some(rhs))

  extension [T[_, _], A, B] (lhs: `Value.Val: x_x_x x x`[T, A, B])
    // def implement(rhs: `Value.Lam.1: x_x_x x x`[A, B]): `Value.Val: x_x_x x x`[T, A, B] =
    //   `Value.Val: x_x_x x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))
    // def implement(rhs: `Value.Val: x_x_x x x`[T, A, B]): `Value.Val: x_x_x x x`[T, A, B] =
    //   `Value.Val: x_x_x x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))
    def implement(rhs: `Value: x_x_x x x`[T, A, B]): `Value.Val: x_x_x x x`[T, A, B] =
      `Value.Val: x_x_x x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T] (lhs: `Value.Val: x`[T])
    def implement(rhs: `Value: x`[T]): `Value.Val: x`[T] =
      `Value.Val: x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T[_], A] (lhs: `Value.Val: x_x x`[T, A])
    def implement(rhs: `Value: x_x x`[T, A]): `Value.Val: x_x x`[T, A] =
      `Value.Val: x_x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T[_], G[_[_], _], H[_], A] (lhs: `Value.Val: x_x llx_xl_x_x x_x xl`[T, G, H, A])
    def implement(rhs: `Value: x_x llx_xl_x_x x_x xl`[T, G, H, A]): `Value.Val: x_x llx_xl_x_x x_x xl`[T, G, H, A] =
      `Value.Val: x_x llx_xl_x_x x_x xl`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))

  extension [T[_[_], _], F[_], A] (lhs: `Value.Val: lx_xl_x_x x_x x`[T, F, A])
    def implement(rhs: `Value: lx_xl_x_x x_x x`[T, F, A]): `Value.Val: lx_xl_x_x x_x x`[T, F, A] =
      `Value.Val: lx_xl_x_x x_x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))


  // extension [A, B] (lhs: `Value.Var.Unbound.Comp.Def.0`[A, B])
  //   def implement(rhs: `Value: x`[A => B]): `Value.Var.Bound.Comp.Def.0`[A, B] =
  //     `Value.Var.Bound.Comp.Def.0`(lhs.lvl, lhs.sym, lhs.tpe, lhs.ret, rhs)

  extension [T[_, _], A, B] (lhs: `Value.Def.1: x_x_x x x`[A, B])
    def implement(rhs: `Value: x`[B]): `Value.Def.1: x_x_x x x`[A, B] =
      `Value.Def.1: x_x_x x x`(lhs.lvl, lhs.sym, lhs.tpe, Some(rhs))