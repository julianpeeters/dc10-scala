package dc10.scala.predef.calculus

import dc10.scala.*
import dc10.scala.internal.substitution.substitute

object application:

  extension [T[_], A] (l: `Type.Lam: x→x`[[X] =>> T[X], A])
    def apply(a: `Type: x`[A]): `Type: x→x x`[T, A] =
      l.codomain.substitute(a)

  extension [T[_[_], _], F[_], A] (l: `Type.Lam: (x→x)→x→x`[[X[_], Y] =>> T[X, Y], F, A])
    def apply(f: `Type: x→x`[F], a: `Type: x`[A]): `Type: (x→x)→x→x x→x x`[T, F, A] =
      l.codomain.substitute(f, a)

  extension [T[_]] (f: `Type: x→x`[T])
    def apply[A](a: `Type: x`[A]): `Type.App: x→x x`[T, A] =
      `Type.App: x→x x`(0, f, a)

  extension [T[_]] (f: `Type: x→x`[T])
    def apply[G[_[_], _], H[_], A](a: `Type: (x→x)→x→x x→x x`[G, H, A]): `Type.App: x→x ((x→x)→x→x x→x x)`[T, G, H, A] =
      `Type.App: x→x ((x→x)→x→x x→x x)`(0, f, a)

  extension [T[_[_]]] (tpe: `Type: (x→x)→x`[T])
    def apply[G[_]](farg: `Type: x→x`[G]): `Type.App: (x→x)→x x→x`[T, G] =
      `Type.App: (x→x)→x x→x`(0, tpe, farg)

  extension [T[_[_], _]] (tpe: `Type: (x→x)→x→x`[T])
    def apply[G[_]](farg: `Type: x→x`[G]): `Type.App: (x→x)→x→x x→x`[T, G] =
      `Type.App: (x→x)→x→x x→x`(0, tpe, farg)

  extension [T[_[_], _]] (tpe: `Type: (x→x)→x→x`[T])
    def apply[G[_], A](farg: `Type: x→x`[G], aarg: `Type: x`[A]): `Type: (x→x)→x→x x→x x`[T, G, A] =
      `Type.App: (x→x)→x→x x→x x`(0, tpe, farg, aarg)

  extension [T[_[_], _]] (tpe: `Type: (x→x)→x→x`[T])
    def apply[G[_], K[_[_]], L[_]](farg: `Type: x→x`[G], aarg: `Type: (x→x)→x x→x`[K, L]): `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[T, G, K, L] =
      `Type.App: (x→x)→x→x x→x ((x→x)→x x→x)`(0, tpe, farg, aarg)

  extension [T[_[_], _], G[_], A] (f: `Type: (x→x)→x→x x→x`[T, G])
    def apply(a: `Type: x`[A]): `Type.App: (x→x)→x→x x→x x`[T, G, A] =
      `Type.App: (x→x)→x→x x→x x`(0, f.tfun, f.targ1, a)

  extension [A, B] (f: `Value: x→x→x x x`[Function1, A, B])
    def apply(a: `Value: x`[A]): `Value.App.1: x`[A, B] =
      `Value.App.1: x`(0, f, a, f.tpe.targ2)

  extension [G[_], A, B] (f: `Value: x→x→x x (x→x x)`[Function1, G, A, B])
    def apply(a: `Value: x`[A]): `Value.App.1: x→x x`[G, A, B] =
      `Value.App.1: x→x x`(0, f, a, f.tpe.targ2)

  extension [F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]] (f: `Value.Def.1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[G, H, A, I, J, K, L])
    def apply(a: `Value: (x→x)→x→x x→x x`[G, H, A]): `Value.App.1: (x→x)→x→x x→x ((x→x)→x x→x)`[G, H, A, I, J, K, L] =
      `Value.App.1: (x→x)→x→x x→x ((x→x)→x x→x)`(0, f, a, f.tpe.targ2)

  // extension [G[_[_], _], H[_], A, B] (f: `Value: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B])
  //   def apply(a: `Value: x`[A]): `Value.App.1: (x→x)→x→x x→x x`[G, H, A, B] =
  //     `Value.App.1: (x→x)→x→x x→x x`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], A, B] (a: `Type: x→x`[G])
    def dot(f: `Value: x→x→x x (x→x x)`[Function1, G, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: x→x x`[G, A, B] =
      `Value.AppDot.1: x→x x`(0, f, a, arg, f.tpe.targ2)

  // extension [G[_], H[_], A, B] (a: `Type: x→x`[G])
  //   def dot(f: `Value: x→x→x x x`[Function1, A, G[B]])(arg: `Value: x`[A]): `Value.AppDot.1: x→x x`[G, A, B] =
  //     `Value.AppDot.1: x→x x`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type: (x→x)→x→x x→x`[G, H])
    def dot[A, B](f: `Value: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: (x→x)→x→x x→x x`[G, H, A, B] =
      `Value.AppDot.1: (x→x)→x→x x→x x`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type: (x→x)→x→x x→x`[G, H])
    def dot[I[_], A, B](f: `Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, I, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: x→x ((x→x)→x→x x→x x)`[I, G, H, A, B] =
      `Value.AppDot.1: x→x ((x→x)→x→x x→x x)`(0, f, a, arg, f.tpe.targ2)

  // extension [G[_], H[_], I[_[_], _], A, B] (a: `Value: x→x x`[G, A])
  //   def dot(f: `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B]): `Value.AppDot.0: (x→x)→x→x x→x x`[G, H, I, A, B] =
  //     `Value.AppDot.0: (x→x)→x→x x→x x`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], I[_[_], _], A, B] (a: `Value: x→x x`[G, A])
    def dot(f: `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B]): `Value.AppDot.0: (x→x)→x→x x→x x`[G, H, I, A, B] =
      `Value.AppDot.0: (x→x)→x→x x→x x`(0, f, a, f.tpe.targ2)

  extension [G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_], B] (a: `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[I, J, G, H, A])
    def dot(f: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L])(arg: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L]) =
      `Value.AppDot.1: (x→x)→x→x x→x ((x→x)→x x→x)`(0, f, a, arg, f.tpe.targ2)