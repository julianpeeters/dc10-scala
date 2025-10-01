package dc10.scala.internal

import dc10.scala.*
import dc10.scala.predef.calculus.Functions.function1

object construct:

  extension [T] (v: `Value.Expr: *`[T])
    def ctor[A](a: `Value.Expr: *`[A]): `Value.Expr: *`[A => T] =
      `Value.Lam.1: *→*→* * *`(0, a, v, function1(a.tpe, v.tpe))

