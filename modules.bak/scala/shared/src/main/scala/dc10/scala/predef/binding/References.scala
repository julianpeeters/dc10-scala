package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.*

trait References[F[_]]:
  given `refT`[`T.x`[t] <: `Type: x`[t], T]: Conversion[`T.x`[T], F[`T.x`[T]]]
  given `refT[_]`[`T.x_x`[t[_]] <: `Type: x_x`[t], T[_]]: Conversion[`T.x_x`[T], F[`T.x_x`[T]]]
  given `refT[_[_]]`[`T.lx_xl_x`[t[_[_]]] <: `Type: lx_xl_x`[t], T[_[_]]]: Conversion[`T.lx_xl_x`[T], F[`T.lx_xl_x`[T]]]
  given `refT[_[_], _]`[`T.lx_xl_x_x`[t[_[_], _]] <: `Type: lx_xl_x_x`[t], T[_[_], _]]: Conversion[`T.lx_xl_x_x`[T], F[`T.lx_xl_x_x`[T]]]
  given `refV`[`V.x`[t] <: `Value: x`[t], T]: Conversion[`V.x`[T], F[`V.x`[T]]]

object References:

  trait Mixins extends References[StateT[ErrorF, Γ, _]]:

    given `refT`[
      `T.x`[t] <: `Type: x`[t],
      T
    ]: Conversion[`T.x`[T], StateT[ErrorF, Γ, `T.x`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[
      `T.x_x`[t[_]] <: `Type: x_x`[t],
      T[_]
    ]: Conversion[`T.x_x`[T], StateT[ErrorF, Γ, `T.x_x`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_]]`[
      `T.lx_xl_x`[t[_[_]]] <: `Type: lx_xl_x`[t],
      T[_[_]]
    ]: Conversion[`T.lx_xl_x`[T], StateT[ErrorF, Γ, `T.lx_xl_x`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[
      `T.lx_xl_x_x`[t[_[_], _]] <: `Type: lx_xl_x_x`[t],
      T[_[_], _]
    ]: Conversion[`T.lx_xl_x_x`[T], StateT[ErrorF, Γ, `T.lx_xl_x_x`[T]]] =
      t => StateT.pure(t)

    given `refV`[
      `V.x`[t] <: `Value: x`[t],
      T
    ]: Conversion[`V.x`[T], StateT[ErrorF, Γ, `V.x`[T]]] =
      v => StateT.pure(v)