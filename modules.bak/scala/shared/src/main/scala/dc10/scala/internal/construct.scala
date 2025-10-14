package dc10.scala.internal

import dc10.scala.*
import dc10.scala.predef.calculus.Functions.function1

object construct:

  extension [T] (v: `Value: x`[T])
    def ctor[A](a: `Value: x`[A]): `Value: x`[A => T] =
      `Value.Lam.1: x_x_x x x`(0, a, v, function1(a.tpe, v.tpe))

