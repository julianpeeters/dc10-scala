package dc10.scala.internal

import dc10.scala.*

object substitution:
  
  extension [F[_], A] (app: `Type.App: x→x x`[F, A])
    def substitute(a: `Type: x`[A]): `Type.App: x→x x`[F, A] =
      app.copy(targ1 = a)

  extension [T[_[_], _], F[_], A] (app: `Type.App: (x→x)→x→x x→x x`[T, F, A])
    def substitute(f: `Type: x→x`[F], a: `Type: x`[A]): `Type.App: (x→x)→x→x x→x x`[T, F, A] =
      app.copy(targ1 = f, targ2 = a)