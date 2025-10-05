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

  extension [T[_]] (f: `Type.Expr: *→*`[T])
    def apply[A](a: `Type: *`[A]): `Type.App: *→* *`[T, A] =
      `Type.App: *→* *`(0, f, a)

  extension [T[_]] (f: `Type.Expr: *→*`[T])
    def apply[G[_[_], _], H[_], A](a: `Type.Expr: (*→*)→*→* *→* *`[G, H, A]): `Type.App: *→* ((*→*)→*→* *→* *)`[G, T, H, A] =
      `Type.App: *→* ((*→*)→*→* *→* *)`(0, f, a)

  extension [T[_[_], _]] (tpe: `Type.Expr: (*→*)→*→*`[T])
    def apply[G[_]](farg: `Type.Expr: *→*`[G]): `Type.App: (*→*)→*→* *→*`[T, G] =
      `Type.App: (*→*)→*→* *→*`(0, tpe, farg)

  extension [T[_[_], _]] (tpe: `Type.Expr: (*→*)→*→*`[T])
    def apply[G[_], A](farg: `Type.Expr: *→*`[G], aarg: `Type.Expr: *`[A]): `Type.Expr: (*→*)→*→* *→* *`[T, G, A] =
      `Type.App: (*→*)→*→* *→* *`(0, tpe, farg, aarg)

  extension [T[_[_], _], G[_], A] (f: `Type.Expr: (*→*)→*→* *→*`[T, G])
    def apply(a: `Type.Expr: *`[A]): `Type.App: (*→*)→*→* *→* *`[T, G, A] =
      `Type.App: (*→*)→*→* *→* *`(0, f.tfun, f.targ1, a)

  extension [A, B] (f: `Value.Expr: *→*→* * *`[Function1, A, B])
    def apply(a: `Value.Expr: *`[A]): `Value.App.1: *`[A, B] =
      `Value.App.1: *`(0, f, a, f.tpe.targ2)

  extension [G[_], A, B] (f: `Value.Expr: *→*→* * (*→* *)`[Function1, G, A, B])
    def apply(a: `Value.Expr: *`[A]): `Value.App.1: *→* *`[G, A, B] =
      `Value.App.1: *→* *`(0, f, a, f.tpe.targ2)

  // extension [G[_[_], _], H[_], A, B] (f: `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B])
  //   def apply(a: `Value.Expr: *`[A]): `Value.App.1: (*→*)→*→* *→* *`[G, H, A, B] =
  //     `Value.App.1: (*→*)→*→* *→* *`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], A, B] (a: `Type.Expr: *→*`[G])
    def dot(f: `Value.Expr: *→*→* * (*→* *)`[Function1, G, A, B])(arg: `Value.Expr: *`[A]): `Value.AppDot.1: *→* *`[G, A, B] =
      `Value.AppDot.1: *→* *`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type.Expr: (*→*)→*→* *→*`[G, H])
    def dot[A, B](f: `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B])(arg: `Value.Expr: *`[A]): `Value.AppDot.1: (*→*)→*→* *→* *`[G, H, A, B] =
      `Value.AppDot.1: (*→*)→*→* *→* *`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type.Expr: (*→*)→*→* *→*`[G, H])
    def dot[I[_], A, B](f: `Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, I, A, B])(arg: `Value.Expr: *`[A]): `Value.AppDot.1: *→* ((*→*)→*→* *→* *)`[I, G, H, A, B] =
      `Value.AppDot.1: *→* ((*→*)→*→* *→* *)`(0, f, a, arg, f.tpe.targ2)

  extension [G[_], H[_], I[_[_], _], A, B] (a: `Value.Expr: *→* *`[G, A])
    def dot(f: `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B]): `Value.AppDot.0: (*→*)→*→* *→* *`[G, H, I, A, B] =
      `Value.AppDot.0: (*→*)→*→* *→* *`(0, f, a, f.tpe.targ2)