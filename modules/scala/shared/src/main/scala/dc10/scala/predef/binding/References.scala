package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.{*}

trait References[F[_]]:
  given `refT`[T]: Conversion[`Type.*`[T], F[`Type.*`[T]]]
  given `refT[_]`[T[_]]: Conversion[`Type.*->*`[T], F[`Type.*->*`[T]]]
  given `refT[_[_]]`[T[_[_]]]: Conversion[`Type.(*->*)->*`[T], F[`Type.(*->*)->*`[T]]]
  given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type.(*->*)->*->*`[T], F[`Type.(*->*)->*->*`[T]]]
  given `refV`[T]: Conversion[`Value.*`[T], F[`Value.*`[T]]]

object References:

  trait Mixins extends References[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    given `refT`[T]: Conversion[`Type.*`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[T[_]]: Conversion[`Type.*->*`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_]]`[T[_[_]]]: Conversion[`Type.(*->*)->*`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type.(*->*)->*->*`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*`[T]]] =
      t => StateT.pure(t)

    given `refV`[T]: Conversion[`Value.*`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]]] =
      v => StateT.pure(v)