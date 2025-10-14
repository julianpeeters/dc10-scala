package dc10.scala.predef.calculus

import dc10.scala.*
import dc10.scala.internal.substitution.substitute

object application:

  extension [T[_], A] (l: `Type.Lam: x_x`[[X] =>> T[X], A])
    def apply(a: `Type: x`[A]): `Type: x_x x`[T, A] =
      l.codomain.substitute(a)

  extension [T[_[_], _], F[_], A] (l: `Type.Lam: lx_xl_x_x`[[X[_], Y] =>> T[X, Y], F, A])
    def apply(f: `Type: x_x`[F], a: `Type: x`[A]): `Type: lx_xl_x_x x_x x`[T, F, A] =
      l.codomain.substitute(f, a)

  extension [T[_]] (f: `Type: x_x`[T])
    def apply[A](a: `Type: x`[A]): `Type.App: x_x x`[T, A] =
      `Type.App: x_x x`(0, f, a)

  extension [T[_]] (f: `Type: x_x`[T])
    def apply[G[_[_], _], H[_], A](a: `Type: lx_xl_x_x x_x x`[G, H, A]): `Type.App: x_x llx_xl_x_x x_x xl`[T, G, H, A] =
      `Type.App: x_x llx_xl_x_x x_x xl`(0, f, a)

  extension [T[_[_]]] (tpe: `Type: lx_xl_x`[T])
    def apply[G[_]](farg: `Type: x_x`[G]): `Type.App: lx_xl_x x_x`[T, G] =
      `Type.App: lx_xl_x x_x`(0, tpe, farg)

  extension [T[_[_], _]] (tpe: `Type: lx_xl_x_x`[T])
    def apply[G[_]](farg: `Type: x_x`[G]): `Type.App: lx_xl_x_x x_x`[T, G] =
      `Type.App: lx_xl_x_x x_x`(0, tpe, farg)
    def apply[G[_], A](farg: `Type: x_x`[G], aarg: `Type: x`[A]): `Type: lx_xl_x_x x_x x`[T, G, A] =
      `Type.App: lx_xl_x_x x_x x`(0, tpe, farg, aarg)
    def apply[G[_], K[_[_]], L[_]](farg: `Type: x_x`[G], aarg: `Type: lx_xl_x x_x`[K, L]): `Type: lx_xl_x_x x_x llx_xl_x x_xl`[T, G, K, L] =
      `Type.App: lx_xl_x_x x_x llx_xl_x x_xl`(0, tpe, farg, aarg)
    def apply[G[_], K[_[_], _], L[_], B](farg: `Type: x_x`[G], aarg: `Type: lx_xl_x_x x_x x`[K, L, B]): `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[T, G, K, L, B] =
      `Type.App: lx_xl_x_x x_x llx_xl_x_x x_x xl`(0, tpe, farg, aarg)

  extension [T[_[_], _], G[_], A] (f: `Type: lx_xl_x_x x_x`[T, G])
    def apply(a: `Type: x`[A]): `Type.App: lx_xl_x_x x_x x`[T, G, A] =
      `Type.App: lx_xl_x_x x_x x`(0, f.tfun, f.targ1, a)

  extension [A, B] (f: `Value: x_x_x x x`[Function1, A, B])
    def apply(a: `Value: x`[A]): `Value.App.1: x`[A, B] =
      `Value.App.1: x`(0, f, a, f.tpe.targ2)

  extension [G[_], A, B] (f: `Value: x_x_x x lx_x xl`[Function1, G, A, B])
    def apply(a: `Value: x`[A]): `Value.App.1: x_x x`[G, A, B] =
      `Value.App.1: x_x x`(0, f, a, f.tpe.targ2)

  extension [F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]] (f: `Value.Def.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[G, H, A, I, J, K, L])
    def apply(a: `Value: lx_xl_x_x x_x x`[G, H, A]): `Value.App.1: lx_xl_x_x x_x llx_xl_x x_xl`[G, H, A, I, J, K, L] =
      `Value.App.1: lx_xl_x_x x_x llx_xl_x x_xl`(0, f, a, f.tpe.targ2)

  // extension [G[_[_], _], H[_], A, B] (f: `Value: x_x_x x llx_xl_x_x x_x xl`[Function1, G, H, A, B])
  //   def apply(a: `Value: x`[A]): `Value.App.1: lx_xl_x_x x_x x`[G, H, A, B] =
  //     `Value.App.1: lx_xl_x_x x_x x`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], A, B] (a: `Type: x_x`[G])
    def dot(f: `Value: x_x_x x lx_x xl`[Function1, G, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: x_x x`[G, A, B] =
      `Value.AppDot.1: x_x x`(0, f, a, arg, f.tpe.targ2)

  // extension [G[_], H[_], A, B] (a: `Type: x_x`[G])
  //   def dot(f: `Value: x_x_x x x`[Function1, A, G[B]])(arg: `Value: x`[A]): `Value.AppDot.1: x_x x`[G, A, B] =
  //     `Value.AppDot.1: x_x x`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type: lx_xl_x_x x_x`[G, H])
    def dot[A, B](f: `Value: x_x_x x llx_xl_x_x x_x xl`[Function1, G, H, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: lx_xl_x_x x_x x`[G, H, A, B] =
      `Value.AppDot.1: lx_xl_x_x x_x x`(0, f, a, arg, f.tpe.targ2)

  extension [G[_[_], _], H[_]] (a: `Type: lx_xl_x_x x_x`[G, H])
    def dot[I[_], A, B](f: `Value: x_x_x x lx_x llx_xl_x_x x_x xll`[Function1, G, H, I, A, B])(arg: `Value: x`[A]): `Value.AppDot.1: x_x llx_xl_x_x x_x xl`[I, G, H, A, B] =
      `Value.AppDot.1: x_x llx_xl_x_x x_x xl`(0, f, a, arg, f.tpe.targ2)

  // extension [G[_], H[_], I[_[_], _], A, B] (a: `Value: x_x x`[G, A])
  //   def dot(f: `Value: x_x_x lx_x xl llx_xl_x_x x_x xl`[Function1, G, H, I, A, B]): `Value.AppDot.0: lx_xl_x_x x_x x`[G, H, I, A, B] =
  //     `Value.AppDot.0: lx_xl_x_x x_x x`(0, f, a, f.tpe.targ2)

  extension [G[_], H[_], I[_[_], _], A, B] (a: `Value: x_x x`[G, A])
    def dot(f: `Value: x_x_x lx_x xl llx_xl_x_x x_x xl`[Function1, G, H, I, A, B]): `Value.AppDot.0: lx_xl_x_x x_x x`[G, H, I, A, B] =
      `Value.AppDot.0: lx_xl_x_x x_x x`(0, f, a, f.tpe.targ2)

  extension [F[_], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B] (a: `Value: x_x llx_xl_x_x x_x xl`[F, G, H, A])
    def dot(f: `Value: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`[Function1, F, G, H, A, I, J, K, L, B]): `Value.AppDot.0: lx_xl_x_x x_x llx_xl_x_x x_x xl`[F, G, H, A, I, J, K, L, B] =
      `Value.AppDot.0: lx_xl_x_x x_x llx_xl_x_x x_x xl`(0, f, a, f.tpe.targ2)

  extension [F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_], M[_[_], _], N[_], O[_[_]], P[_], B] (a: `Value: lx_xl_x_x x_x llx_xl_x_x x_x xl`[I, J, G, H, A])
    def dot(f: `Value: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`[Function1, F, G, H, A, I, J, K, L, M, N, O, P])(arg: `Value: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F, G, H, A, I, J, K, L]): `Value.AppDot.1_: lx_xl_x_x x_x llx_xl_x x_xl`[F, G, H, A, I, J, K, L, M, N, O, P] =
      `Value.AppDot.1_: lx_xl_x_x x_x llx_xl_x x_xl`(0, f, a, arg, f.tpe.targ2)