package dc10.scala

import cats.{Eval, Functor}
import cats.free.Cofree
import java.nio.file.Path

sealed trait Symbol

object Symbol:

  // Templates ////////////////////////////////////////////////////////////////
  sealed trait Template extends Symbol
  case class Extension(field: Statement, body: List[Statement]) extends Template
  sealed abstract class CaseClass[Z, T] extends Template:
    def nme: String
    def tpe: Term.Type[Z, T]
    def fields: List[Statement]
    def body: List[Statement]

  object CaseClass:
    def apply[T](
      q: Option[Long],
      n: String,
      fs: List[Statement],
    ): CaseClass[Unit, T] =
      new CaseClass[Unit, T]:
        def nme = n
        def tpe: Term.Type[Unit, T] = Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(q, n, None)))
        def fields = fs
        def body = Nil

  // Object ///////////////////////////////////////////////////////////////////
  sealed abstract class Object[Z, T] extends Symbol:
    def nme: String
    def par: Option[Term.Type[Z, T]]
    def tpe: Term.Type[Z, T]
    def body: List[Statement]

  object Object:
    def apply[T](
      q: Option[Long],
      n: String,
      p: Option[Term.Type[Unit, T]],
      b: List[Statement],
    ): Object[Unit, T] =
      new Object[Unit, T]:
        def nme = n
        def par = p
        def tpe: Term.Type[Unit, T] = Cofree((), Eval.now(
          p.fold(
            Term.TypeLevel.Var.UserDefinedType(q, n, None)
          )(i => i.tail.value)
        ))
        def body: List[Statement] = b

  // Package //////////////////////////////////////////////////////////////////
  sealed abstract class Package extends Symbol

  object Package:

    extension (pkg: Package)
      def getPath: Path =
        pkg match
          case Basic(nme, nst) => Path.of(nme).resolve(nst.pkg.getPath)
          case Empty(ms) => Path.of("")

      def addMember(stmt: Statement): Package =
        pkg match
          case Basic(nme, nst) => nst.pkg.addMember(stmt)
          case Empty(ms) => Empty(ms :+ stmt)

    case class Basic(
      nme: String,
      nst: Statement.PackageDef
    ) extends Package

    case class Empty(
      ms : List[Statement]
    ) extends Package

  // Term /////////////////////////////////////////////////////////////////////
  sealed abstract class Term extends Symbol:
    def qnt: Option[Long]

  object Term:

    type Type[Z, T] = Cofree[[X] =>> TypeLevel[T, X], Z]
    sealed trait TypeLevel[T, +X] extends Term
    object TypeLevel:

      given F[X, T]: Functor[[X] =>> TypeLevel[T, X]] =
        new Functor[[X] =>> TypeLevel[T, X]]:
          def map[A, B](fa: TypeLevel[T, A])(f: A => B): TypeLevel[T, B] =
            fa.asInstanceOf[TypeLevel[T, B]] 

      type __
      case class App1[Y, Z, T[_], U, A, X](qnt: Option[Long], tfun: Type[Z, T[U]], targ: Type[Y, A]) extends TypeLevel[T[A], X]
      case class App2[Z, T[_,_], A, B, X](qnt: Option[Long], tfun: Type[Z, T[__,__]], ta: Type[Z, A], tb: Type[Z, B]) extends TypeLevel[T[A, B], X]
      case class App3[Z, T[_,_,_], A, B, X](qnt: Option[Long], tfun: Type[Z, T[__,__,__]], ta1: Type[Z, A], ta2: Type[Z, A], tb: Type[Z, B]) extends TypeLevel[T[A, A, B], X]
      sealed abstract class Var[Z, T, +X] extends TypeLevel[T, X]
      object Var:
        case class BooleanType[Z, X](qnt: Option[Long]) extends Var[Z, Boolean, X]
        case class IntType[Z, X](qnt: Option[Long]) extends Var[Z, Int, X]
        case class StringType[Z, X](qnt: Option[Long]) extends Var[Z, String, X]
        case class Function1Type[Z, X](qnt: Option[Long]) extends Var[Z, __ => __, X]
        case class Function2Type[Z, X](qnt: Option[Long]) extends Var[Z, (__, __) => __, X]
        case class ListType[Z, G[_] <: List[?], X](qnt: Option[Long]) extends Var[Z, G[__], X]
        case class OptionType[Z, G[_] <: Option[?], X](qnt: Option[Long]) extends Var[Z, G[__], X]
        object OptionType:
          case class SomeType[Z, X](qnt: Option[Long]) extends Var[Z, Option[__], X]
        case class UserDefinedType[Z, T, X <: TypeLevel[T, X]](qnt: Option[Long], nme: String, impl: Option[Type[Z, T]]) extends Var[Z, T, X]
    
    type Value[Z, T] = Cofree[[X] =>> ValueLevel[T, X], Z]
   
    sealed trait ValueLevel[T, +X] extends Term:
      type Zed
      def tpe: Type[Zed, T]

    object ValueLevel:

      given F[X, T]: Functor[[X] =>> ValueLevel[T, X]] =
        new Functor[[X] =>> ValueLevel[T, X]]:
          def map[A, B](fa: ValueLevel[T, A])(f: A => B): ValueLevel[T, B] =
            fa.asInstanceOf[ValueLevel[T, B]] 

      sealed abstract class App[Z, T, X] extends Term.ValueLevel[T, X]:
        type Zed = Z
      object App:
        case class App1[Z, A, B, X](qnt: Option[Long], fun: Value[Z, A => B], arg: Value[Z, A], tpe: Type[Z, B]) extends Term.ValueLevel.App[Z, B, X]
        case class AppCtor1[Z, T, A, X](qnt: Option[Long], tpe: Type[Z, T], arg: Value[Z, A]) extends Term.ValueLevel.App[Z, T, X]
        case class AppPure[G[_], Z, A, X](qnt: Option[Long], fun: Value[Z, A => G[A]], arg: Value[Z, A], tpe: Type[Z, G[A]]) extends Term.ValueLevel.App[Z, G[A], X]
        case class AppVargs[G[_], Y, Z, A, B, X](qnt: Option[Long], fun: Value[Z, G[A] => G[A]], vargs: Value[Y, A]*) extends Term.ValueLevel.App[Z, G[A], X]:
          def tpe: Type[Z, G[A]] = ???

        case class Dot1[Z, A, B, C, D, X](qnt: Option[Long], fun: Value[Z, D], arg1: Value[Z, A], arg2: Value[Z, B]) extends Term.ValueLevel.App[Z, C, X]:
          def tpe: Type[Z, C] = ???
      sealed abstract class Lam[Z, T, X] extends Term.ValueLevel[T, X]:
        type Zed = Z
      object Lam:
        case class Lam1[Z, A, B, X](qnt: Option[Long], a: Value[Z, A], b: Value[Z, B]) extends Term.ValueLevel.Lam[Z, A => B, X]:
          def tpe: Type[Z, A => B] = Cofree(???, Eval.now(Term.TypeLevel.App2(None, Cofree(???, Eval.now(Term.TypeLevel.Var.Function1Type(None))), a.tail.value.tpe, ???)))
        case class Lam2[Z, A, B, X](qnt: Option[Long], a1: Value[Z, A], a2: Value[Z, A], b: Value[Z, B]) extends Term.ValueLevel.Lam[Z, (A, A) => B, X]:
          def tpe: Type[Z, (A, A) => B] = ???

      sealed abstract class Var[Z, T, X] extends Term.ValueLevel[T, X]:
        type Zed = Z
      object Var:
        case class BooleanLiteral[X](qnt: Option[Long], b: Boolean) extends Var[Unit, Boolean, X]:
          def tpe: Type[Unit, Boolean] = Cofree((), Eval.now(Term.TypeLevel.Var.BooleanType(None)))
        case class IntLiteral[X](qnt: Option[Long], i: Int) extends Var[Unit, Int, X]:
          def tpe: Type[Unit, Int] = Cofree((), Eval.now(Term.TypeLevel.Var.IntType(None)))
        case class StringLiteral[X](qnt: Option[Long], s: String) extends Var[Unit, String, X]:
          def tpe: Type[Unit, String] = Cofree((), Eval.now(Term.TypeLevel.Var.StringType(None)))
        case class ListCtor[A, X](qnt: Option[Long]) extends Var[Unit, List[A] => List[A], X]:
          def tpe: Type[Unit, List[A] => List[A]] = ???
        case class OptionCtor[A, X](qnt: Option[Long]) extends Var[Unit, A => Option[A], X]:
          def tpe: Type[Unit, A => Option[A]] = ???
        object OptionCtor:
          case class SomeCtor[A, X](qnt: Option[Long]) extends Var[Unit, A => Some[A], X]:
            def tpe: Type[Unit, A => Some[A]] = Cofree((), Eval.now(Term.TypeLevel.App2(None, Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))), Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "A", None))), Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "A", None))))))
        case class Println[Z, F[_], X](qnt: Option[Long], s: Value[Unit, String]) extends Var[Z, F[Unit], X]:
          def tpe: Type[Z, F[Unit]] = ???//Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "cats.effect.IO[Unit]", None)))
        case class UserDefinedValue[Z, T, X <: ValueLevel[T, X]](qnt: Option[Long], nme: String, tpe: Type[Z, T], impl: Option[Value[Z, T]]) extends Var[Z, T, X]