package dc10.scala

sealed trait Type
sealed trait `Type.*`[+T] extends Type
sealed trait `Type.*->*`[T[_]] extends Type
sealed trait `Type.*->*->*`[T[_, _]] extends Type
sealed trait `Type.*->*->*->*`[T[_, _, _]] extends Type
sealed trait `Type.*->*->*->*->*`[T[_, _, _, _]] extends Type
sealed trait `Type.(*->*)->*`[T[_[_]]] extends Type
sealed trait `Type.(*->*)->*->*`[T[_[_], _]] extends Type
sealed trait `Type.(*->*)->*->*->*`[T[_[_], _, _]] extends Type
sealed trait `Type.((*->*)->*->*)->*`[T[_[_[_], _]]] extends Type

object Type:

  type __
  
  case class `App[_]`[T[_], A](indent: Int, tfun: `Type.*->*`[T], aarg: `Type.*`[A]) extends `Type.*`[T[A]]
  case class `App[_[_]]`[T[_[_]], F[_]](indent: Int, tfun: `Type.(*->*)->*`[T], farg: `Type.*->*`[F]) extends `Type.*`[T[F]]
  case class `App[_[_[_], _]]`[T[_[_[_], _]], F[_[_], _]](indent: Int, tfun: `Type.((*->*)->*->*)->*`[T], farg: `Type.(*->*)->*->*`[F]) extends `Type.*`[T[F]]
  case class `App[_, _]`[T[_,_], A, B](indent: Int, tfun: `Type.*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[A, B]]
  case class `App[_[_], _]`[T[_[_], _], F[_], A](indent: Int, tfun: `Type.(*->*)->*->*`[T], farg: `Type.*->*`[F], aarg: `Type.*`[A]) extends `Type.*`[T[F, A]]
  case class `App[_, _, _]`[T[_,_,_], A, B, C](indent: Int, tfun: `Type.*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C]) extends `Type.*`[T[A, B, C]]
  case class `App[_[_], _, _]`[T[_[_], _, _], F[_], A, B](indent: Int, tfun: `Type.(*->*)->*->*->*`[T], farg: `Type.*->*`[F], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[F, A, B]]
  case class `App[_, _, _, _]`[T[_,_,_,_], A, B, C, D](indent: Int, tfun: `Type.*->*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C], darg: `Type.*`[D]) extends `Type.*`[T[A, B, C, D]]
  case class `AppInfix[_, _]`[T[_,_], A, B](indent: Int, tfun: `Type.*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[A, B]]
  case class `AppInfix[_, _, _]`[T[_,_,_], A, B, C](indent: Int, tfun: `Type.*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C]) extends `Type.*`[T[A, B, C]]
  case class `AppInfix[_, _, _, _]`[T[_,_,_,_], A, B, C, D](indent: Int, tfun: `Type.*->*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C], darg: `Type.*`[D]) extends `Type.*`[T[A, B, C, D]]
  case class `Lam`[F[_], A](indent: Int, domain: Type.`Var`[A], codomain: `Type.*`[F[A]]) extends `Type.*->*`[F]
  case class `Var`[T](indent: Int, nme: String, impl: Option[`Type.*`[T]]) extends `Type.*`[T]
  case class `Var[_]`[T[_]](indent: Int, nme: String, impl: Option[`Type.*->*`[T]]) extends `Type.*->*`[T]
  case class `Var[_[_]]`[T[_[_]]](indent: Int, nme: String, impl: Option[`Type.(*->*)->*`[T]]) extends `Type.(*->*)->*`[T]
  case class `Var[_, _]`[T[_, _]](indent: Int, nme: String, impl: Option[`Type.*->*->*`[T]]) extends `Type.*->*->*`[T]
  case class `Var[_[_], _]`[T[_[_], _]](indent: Int, nme: String, impl: Option[`Type.(*->*)->*->*`[T]]) extends `Type.(*->*)->*->*`[T]
  case class `Var[_, _, _]`[T[_, _, _]](indent: Int, nme: String, impl: Option[`Type.*->*->*->*`[T]]) extends `Type.*->*->*->*`[T]
  case class `Var[_[_], _, _]`[T[_[_], _, _]](indent: Int, nme: String, impl: Option[`Type.(*->*)->*->*->*`[T]]) extends `Type.(*->*)->*->*->*`[T]
  case class `Var[_[_[_], _]]`[T[_[_[_], _]]](indent: Int, nme: String, impl: Option[`Type.((*->*)->*->*)->*`[T]]) extends `Type.((*->*)->*->*)->*`[T]
  case class `Var[_, _, _, _]`[T[_, _, _, _]](indent: Int, nme: String, impl: Option[`Type.*->*->*->*->*`[T]]) extends `Type.*->*->*->*->*`[T]
  case class `Bot`(indent: Int) extends `Type.*`[Nothing]

  extension [T] (t: `Type.*`[T])
    def addIndent: `Type.*`[T] =
      t match
        case `App[_]`(indent, tfun, aarg) => `App[_]`(indent + 1, tfun, aarg)
        case `App[_[_]]`(indent, tfun, farg) => `App[_[_]]`(indent + 1, tfun, farg)
        case `App[_, _]`(indent, tfun, aarg, barg) => `App[_, _]`(indent + 1, tfun, aarg, barg)
        case `App[_[_], _]`(indent, tfun, farg, aarg) => `App[_[_], _]`(indent + 1, tfun, farg, aarg)
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => `App[_, _, _]`(indent + 1, tfun, aarg, barg, carg)
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => `App[_[_], _, _]`(indent + 1, tfun, farg, aarg, barg)
        case `App[_[_[_], _]]`(indent, tfun, farg) => `App[_[_[_], _]]`(indent + 1, tfun, farg)
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => `App[_, _, _, _]`(indent + 1, tfun, aarg, barg, carg, darg)
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) => `AppInfix[_, _]`(indent + 1, tfun, aarg, barg)
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => `AppInfix[_, _, _]`(indent + 1, tfun, aarg, barg, carg)
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => `AppInfix[_, _, _, _]`(indent + 1, tfun, aarg, barg, carg, darg)
        case `Var`(indent, nme, impl) => `Var`(indent + 1, nme, impl)
        case `Bot`(indent) => `Bot`(indent + 1)

    def assign[A](rhs: `Type.*`[A]): Either[List[Error], Type.Var[A]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Var`(indent, nme, impl) => impl.fold(Right(`Var`(indent, nme, Some(rhs))))(_ => Left(List(Error(s"Type is already assigned ${t}"))))
        case `Bot`(indent) => Left(List(Error(s"Type is not assignable ${t}")))

    def assign[F[_]](rhs: `Type.*->*`[F]): Either[List[Error], Type.`Var[_]`[F]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Var`(indent, nme, impl) => impl.fold(Right(`Var[_]`(indent, nme, Some(rhs))))(_ => Left(List(Error(s"Type is already assigned ${t}"))))
        case `Bot`(indent) => Left(List(Error(s"Type is not assignable ${t}")))

    def substitute[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[A]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Right(`App[_]`(indent, tfun, a).asInstanceOf[`Type.*`[A]])
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not substitutable ${t}")))
        case `Var`(indent, nme, impl) => Right(a)
        case `Bot`(indent) => Left(List(Error(s"Type is not substitutable ${t}")))
      
  extension [A, B] (t: `Type.*`[A => B])
    def unapplyRightmost: Either[List[Error], `Type.*`[B]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Right(aarg.asInstanceOf[`Type.*`[B]])
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) => Right(barg.asInstanceOf[`Type.*`[B]])
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Var`(indent, nme, impl) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Bot`(indent) => Left(List(Error(s"Type is not a functor ${t}")))

  extension [T] (t: `Type.*`[T])
    def returnType[B]: Either[List[Error], `Type.*`[B]] =
      t match
        case Type.`App[_]`(indent, tfun, aarg) => Right(aarg.asInstanceOf[`Type.*`[B]])// Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`AppInfix[_, _]`(indent, tfun, aarg, barg) => Right(barg.asInstanceOf[`Type.*`[B]])
        case Type.`AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a function type ${t}")))
        case Type.`Var`(indent, nme, impl) => Left(List(Error(s"not a function type ${t}")))
        case `Bot`(indent) => Left(List(Error(s"not a function type ${t}")))


        

  extension [T[_], A] (t: `Type.*`[A => T[A]])
    @scala.annotation.targetName("substitute A => T[A]")
    def substitute(a: `Type.*`[A]): Either[List[Error], `Type.*`[A => T[A]]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) =>
          for
            a <- aarg.substitute(a)
            b <- barg.substitute(a)
          yield `AppInfix[_, _]`(indent, tfun, a, b)
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Var`(indent, nme, impl) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Bot`(indent) => Left(List(Error(s"type does not support substitution ${t}")))

  extension [T[_], A] (t: `Type.*`[List[A] => T[A]])
    @scala.annotation.targetName("substitute List[A] => T[A]")
    def substitute(a: `Type.*`[A]): Either[List[Error], `Type.*`[List[A] => T[A]]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) =>
          for
            aa <- aarg.asInstanceOf[`Type.*`[List[A]]].substitute(a)
            bb <- barg.asInstanceOf[`Type.*`[T[A]]].substitute(a)
          yield `AppInfix[_, _]`(indent, tfun, aa, bb)
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Var`(indent, nme, impl) => Left(List(Error(s"type does not support substitution ${t}")))
        case `Bot`(indent) => Left(List(Error(s"type does not support substitution ${t}")))

  extension [T[_], A] (t: `Type.*`[T[A]])
    @scala.annotation.targetName("substitute T[A]")
    def substitute(a: `Type.*`[A]): Either[List[Error], `Type.*`[T[A]]] =
      t match
        case `App[_]`(indent, tfun, aarg) => Right(`App[_]`(indent, tfun, a))
        case `App[_[_]]`(indent, tfun, farg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_[_[_], _]]`(indent, tfun, farg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_, _]`(indent, tfun, aarg, barg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_[_], _]`(indent, tfun, farg, aarg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_[_], _, _]`(indent, tfun, farg, aarg, barg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `App[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `AppInfix[_, _]`(indent, tfun, aarg, barg) =>
          for
            a <- aarg.substitute(a)
            b <- barg.substitute(a)
          yield `AppInfix[_, _]`(indent, tfun, a, b).asInstanceOf[`Type.*`[T[A]]]
        case `AppInfix[_, _, _]`(indent, tfun, aarg, barg, carg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `AppInfix[_, _, _, _]`(indent, tfun, aarg, barg, carg, darg) => Left(List(Error(s"not a substitutable parameterized type ${t}")))
        case `Var`(indent, nme, impl) => Left(List(Error(s"not a substitutable parameterized type type ${t}")))
        case `Bot`(indent) => Left(List(Error(s"not a substitutable parameterized type type ${t}")))

  extension [T[_]] (f: `Type.*->*`[[A] =>> T[A]])
    def apply[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[T[A]]] =
      f match
        case Type.`Lam`(indent, domain, codomain) => codomain.asInstanceOf[`Type.*`[T[A]]].substitute(a)
        case Type.`Var[_]`(indent, nme, impl) => impl.fold(Left(List(Error(s"unimplemented type does not support substitution ${f}"))))(i => i(a))

  extension [T[_]] (f: `Type.*->*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply [A] =>> A => T[A]")
    def apply[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[A => T[A]]] =
      f match
        case `Lam`(indent, domain, codomain) => codomain.asInstanceOf[`Type.*`[A => T[A]]].substitute(a)
        case `Var[_]`(indent, nme, impl) => ???


  extension [T[_]] (f: `Type.*->*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply [A] =>> List[A] => T[A]")
    def applyVargs[A](a: `Type.*`[A]): Either[List[Error], `Type.*`[List[A] => T[A]]] =
      f match
        case Type.Lam(indent, domain, codomain) =>
          codomain.asInstanceOf[`Type.*`[List[A] => T[A]]].substitute(a)
       
        case Type.`Var[_]`(indent, nme, impl) => ???
      
      
      


  extension [T] (t: Type.Var[T])
    def addIndent: Type.Var[T] =
      t match
        case Var(indent, nme, impl) => Var(indent + 1, nme, impl)

  extension [T[_]] (t: Type.`Var[_]`[T])
    def addIndent: Type.`Var[_]`[T] =
      t match
        case `Var[_]`(indent, nme, impl) => `Var[_]`(indent + 1, nme, impl)
  
  extension [T[_[_]]] (t: Type.`Var[_[_]]`[T])
    def addIndent: Type.`Var[_[_]]`[T] =
      t match
        case `Var[_[_]]`(indent, nme, impl) => `Var[_[_]]`(indent + 1, nme, impl)

  extension [T[_, _]] (t: Type.`Var[_, _]`[T])
    def addIndent: Type.`Var[_, _]`[T] =
      t match
        case `Var[_, _]`(indent, nme, impl) => `Var[_, _]`(indent + 1, nme, impl)

  extension [T[_[_], _]] (t: Type.`Var[_[_], _]`[T])
    def addIndent: Type.`Var[_[_], _]`[T] =
      t match
        case `Var[_[_], _]`(indent, nme, impl) => `Var[_[_], _]`(indent + 1, nme, impl)

  extension [T[_[_], _, _]] (t: Type.`Var[_[_], _, _]`[T])
    def addIndent: Type.`Var[_[_], _, _]`[T] =
      t match
        case `Var[_[_], _, _]`(indent, nme, impl) => `Var[_[_], _, _]`(indent + 1, nme, impl)
        
      
    
