package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.*

trait References[F[_]]:
  given `refT`[`T.*`[t] <: `Type.Expr: *`[t], T]: Conversion[`T.*`[T], F[`T.*`[T]]]
  given `refT[_]`[`T.*→*`[t[_]] <: `Type: *→*`[t], T[_]]: Conversion[`T.*→*`[T], F[`T.*→*`[T]]]
  given `refT[_[_]]`[`T.(*→*)→*`[t[_[_]]] <: `Type: (*→*)→*`[t], T[_[_]]]: Conversion[`T.(*→*)→*`[T], F[`T.(*→*)→*`[T]]]
  given `refT[_[_], _]`[`T.(*→*)→*→*`[t[_[_], _]] <: `Type: (*→*)→*→*`[t], T[_[_], _]]: Conversion[`T.(*→*)→*→*`[T], F[`T.(*→*)→*→*`[T]]]
  given `refV`[`V.*`[t] <: `Value.Expr: *`[t], T]: Conversion[`V.*`[T], F[`V.*`[T]]]

object References:

  trait Mixins extends References[StateT[ErrorF, Γ, _]]:

    given `refT`[
      `T.*`[t] <: `Type.Expr: *`[t],
      T
    ]: Conversion[`T.*`[T], StateT[ErrorF, Γ, `T.*`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[
      `T.*→*`[t[_]] <: `Type: *→*`[t],
      T[_]
    ]: Conversion[`T.*→*`[T], StateT[ErrorF, Γ, `T.*→*`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_]]`[
      `T.(*→*)→*`[t[_[_]]] <: `Type: (*→*)→*`[t],
      T[_[_]]
    ]: Conversion[`T.(*→*)→*`[T], StateT[ErrorF, Γ, `T.(*→*)→*`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[
      `T.(*→*)→*→*`[t[_[_], _]] <: `Type: (*→*)→*→*`[t],
      T[_[_], _]
    ]: Conversion[`T.(*→*)→*→*`[T], StateT[ErrorF, Γ, `T.(*→*)→*→*`[T]]] =
      t => StateT.pure(t)

    given `refV`[
      `V.*`[t] <: `Value.Expr: *`[t],
      T
    ]: Conversion[`V.*`[T], StateT[ErrorF, Γ, `V.*`[T]]] =
      v => StateT.pure(v)