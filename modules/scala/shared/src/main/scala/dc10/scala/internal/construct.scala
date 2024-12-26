package dc10.scala.internal

import dc10.scala.*
import dc10.scala.internal.extract.unpure
import dc10.scala.internal.indent.getIndent
import dc10.scala.internal.substitute.sub

object construct:

  extension [T[_]] (f: `Type.*->*`[[A] =>> T[A]])
    def applyType[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[T[A]]] =
      f match
        case `Type.Lam`(in, domain, codomain) => codomain.asInstanceOf[`Type.*`[T[A]]].sub(a)
        case `Type.Var[_]`(in, nme, impl) => impl.fold(Left(List(Error(s"unimplemented type does not support substitution ${f}"))))(i => i.applyType(a))

  extension [T[_]] (f: `Type.*->*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply [A] =>> A => T[A]")
    def applyType[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[A => T[A]]] =
      f match
        case `Type.Lam`(in, domain, codomain) => codomain.asInstanceOf[`Type.*`[A => T[A]]].sub(a)
        case `Type.Var[_]`(in, nme, impl) => ???

  extension [T[_]] (f: `Type.*->*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply [A] =>> List[A] => T[A]")
    def applyType[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[List[A] => T[A]]] =
      f match
        case `Type.Lam`(in, domain, codomain) =>
          codomain.asInstanceOf[`Type.*`[List[A] => T[A]]].sub(a)
        case `Type.Var[_]`(in, nme, impl) => ???

  extension [T[_]] (v: `Value.*->*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply T [A] =>> A => T[A]")
    def applyValue[A](targ: `Type.*`[A]): Either[List[Error], `Value.*`[A => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.AppType`(v.getIndent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> A => T[A]")
    def applyValue[A](a: `Value.*`[A]): Either[List[Error], `Value.*`[T[A]]] =
      for
        c <- v.applyValue(a.tpe)
        t <- v.tpe.applyType(a.tpe)
        r <- t.unpure
      yield `Value.App1`(v.getIndent, c, a, r)

  extension [T[_]] (v: `Value.*->*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply T [A] =>> List[A] => T[A]")
    def applyValue[A](targ: `Type.*`[A]): Either[List[Error], `Value.*`[List[A] => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.AppType`(v.getIndent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> List[A] => T[A]")
    def applyValue[A](a: `Value.*`[A]*): Either[List[Error], `Value.*`[T[A]]] =
      for
        x <- a.toList.headOption.fold(Right(`Type.Bot`(v.getIndent)))(a => Right(a.tpe))
        c <- v.applyValue(x)
        t <- v.tpe.applyType(x)
        r <- t.unpure
      yield `Value.AppVargs`[A, T[A]](v.getIndent, c, r, a*)

  extension [T] (v: `Value.*`[T])
    def ctor[A](a: `Value.*`[A]): `Value.*`[A => T] =
      `Value.Lam1`(0, a, v, `Type.AppInfix[_, _]`(
            0,
            `Type.Var[_, _]`[Function1](0, "=>", None),
            a.tpe,
            v.tpe,
          )
        )