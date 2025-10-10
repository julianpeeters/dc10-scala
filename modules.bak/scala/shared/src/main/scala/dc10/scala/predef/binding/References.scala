package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.*

trait References[F[_]]:
  given `refT`[`T.x`[t] <: `Type: x`[t], T]: Conversion[`T.x`[T], F[`T.x`[T]]]
  given `refT[_]`[`T.x→x`[t[_]] <: `Type: x→x`[t], T[_]]: Conversion[`T.x→x`[T], F[`T.x→x`[T]]]
  given `refT[_[_]]`[`T.(x→x)→x`[t[_[_]]] <: `Type: (x→x)→x`[t], T[_[_]]]: Conversion[`T.(x→x)→x`[T], F[`T.(x→x)→x`[T]]]
  given `refT[_[_], _]`[`T.(x→x)→x→x`[t[_[_], _]] <: `Type: (x→x)→x→x`[t], T[_[_], _]]: Conversion[`T.(x→x)→x→x`[T], F[`T.(x→x)→x→x`[T]]]
  given `refV`[`V.x`[t] <: `Value: x`[t], T]: Conversion[`V.x`[T], F[`V.x`[T]]]

object References:

  trait Mixins extends References[StateT[ErrorF, Γ, _]]:

    given `refT`[
      `T.x`[t] <: `Type: x`[t],
      T
    ]: Conversion[`T.x`[T], StateT[ErrorF, Γ, `T.x`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[
      `T.x→x`[t[_]] <: `Type: x→x`[t],
      T[_]
    ]: Conversion[`T.x→x`[T], StateT[ErrorF, Γ, `T.x→x`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_]]`[
      `T.(x→x)→x`[t[_[_]]] <: `Type: (x→x)→x`[t],
      T[_[_]]
    ]: Conversion[`T.(x→x)→x`[T], StateT[ErrorF, Γ, `T.(x→x)→x`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[
      `T.(x→x)→x→x`[t[_[_], _]] <: `Type: (x→x)→x→x`[t],
      T[_[_], _]
    ]: Conversion[`T.(x→x)→x→x`[T], StateT[ErrorF, Γ, `T.(x→x)→x→x`[T]]] =
      t => StateT.pure(t)

    given `refV`[
      `V.x`[t] <: `Value: x`[t],
      T
    ]: Conversion[`V.x`[T], StateT[ErrorF, Γ, `V.x`[T]]] =
      v => StateT.pure(v)