package dc10.scala.internal

import dc10.scala.*

object substitute:

  extension [T] (t: `Type.*`[T])
    def sub[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[A]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Right(`Type.App[_]`(in, tfun, a).asInstanceOf[`Type.*`[A]])
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Type.Var`(in, nme, impl) => Right(a)
        case `Type.Bot`(in) => Left(List(Error(s"Type is not substitutable ${t}")))
     
  extension [T[_], A] (t: `Type.*`[T[A]])
    @scala.annotation.targetName("sub T[A]")
    def sub(a: `Type.*`[A]): Either[List[Error], `Type.*`[T[A]]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Right(`Type.App[_]`(in, tfun, a))
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) =>
          for
            a <- aarg.sub(a)
            b <- barg.sub(a)
          yield `Type.AppInfix[_, _]`(in, tfun, a, b).asInstanceOf[`Type.*`[T[A]]]
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Type.Var`(in, nme, impl) => Left(List(Error(s"not a substitutable parameterized type type ${t}")))
        case `Type.Bot`(in) => Left(List(Error(s"not a substitutable parameterized type type ${t}")))

  extension [T[_], A] (t: `Type.*`[A => T[A]])
    @scala.annotation.targetName("sub A => T[A]")
    def sub(a: `Type.*`[A]): Either[List[Error], `Type.*`[A => T[A]]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) =>
          for
            a <- aarg.sub(a)
            b <- barg.sub(a)
          yield `Type.AppInfix[_, _]`(in, tfun, a, b)
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.Var`(in, nme, impl) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.Bot`(in) => Left(List(Error(s"type does not support substitution ${t}")))

  extension [T[_], A] (t: `Type.*`[List[A] => T[A]])
    @scala.annotation.targetName("sub List[A] => T[A]")
    def sub(a: `Type.*`[A]): Either[List[Error], `Type.*`[List[A] => T[A]]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) =>
          for
            aa <- aarg.asInstanceOf[`Type.*`[List[A]]].sub(a)
            bb <- barg.asInstanceOf[`Type.*`[T[A]]].sub(a)
          yield `Type.AppInfix[_, _]`(in, tfun, aa, bb)
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.Var`(in, nme, impl) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Type.Bot`(in) => Left(List(Error(s"type does not support substitution ${t}")))