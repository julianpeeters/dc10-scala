package dc10.scala.internal

import dc10.scala.*

object substitution:
  
  extension [F[_], A] (app: `Type.App: *→* *`[F, A])
    def substitute(a: `Type.Expr: *`[A]): `Type.App: *→* *`[F, A] =
      app.copy(targ1 = a)

  extension [T[_[_], _], F[_], A] (app: `Type.App: (*→*)→*→* *→* *`[T, F, A])
    def substitute(f: `Type.Expr: *→*`[F], a: `Type.Expr: *`[A]): `Type.App: (*→*)→*→* *→* *`[T, F, A] =
      app.copy(targ1 = f, targ2 = a)