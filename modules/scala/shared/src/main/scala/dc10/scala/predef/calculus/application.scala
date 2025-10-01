package dc10.scala.predef.calculus

import dc10.scala.*
import dc10.scala.internal.substitution.substitute

object application:

  extension [T[_], A] (l: `Type.Lam: *→*`[[X] =>> T[X], A])
    def apply(a: `Type.Expr: *`[A]): `Type.Expr: *→* *`[T, A] =
      l.codomain.substitute(a)

  extension [T[_[_], _], F[_], A] (l: `Type.Lam: (*→*)→*→*`[[X[_], Y] =>> T[X, Y], F, A])
    def apply(f: `Type.Expr: *→*`[F], a: `Type.Expr: *`[A]): `Type.Expr: (*→*)→*→* *→* *`[T, F, A] =
      l.codomain.substitute(f, a)

  extension [T[_]] (f: `Type.Expr: *→*`[[X] =>> T[X]])
    def apply[A](a: `Type.Expr: *`[A]): `Type.App: *→* *`[T, A] =
      `Type.App: *→* *`(0, f, a)

  extension [T[_[_], _]] (tpe: `Type.Expr: (*→*)→*→*`[T])
    def apply[G[_], A](farg: `Type.Expr: *→*`[G], aarg: `Type.Expr: *`[A]): `Type.Expr: (*→*)→*→* *→* *`[T, G, A] =
      `Type.App: (*→*)→*→* *→* *`(0, tpe, farg, aarg)

  extension [A, B] (f: `Value.Expr: *→*→* * *`[Function1, A, B])
    def apply(a: `Value.Expr: *`[A]): `Value.App.1: *`[A, B] =
      `Value.App.1: *`(0, f, a, f.tpe.targ2)

  extension [G[_], A, B] (f: `Value.Expr: *→*→* * (*→* *)`[Function1, G, A, B])
    def apply(a: `Value.Expr: *`[A]): `Value.App.1: *→* *`[G, A, B] =
      `Value.App.1: *→* *`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], I[_[_], _], A, B] (a: `Value.Expr: *→* *`[G, A])
    def dot(f: `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B]): `Value.AppDot.0: (*→*)→*→* *→* *`[G, H, I, A, B] =
      `Value.AppDot.0: (*→*)→*→* *→* *`(0, f, a, f.tpe.targ2)