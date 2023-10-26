package dc10.scala

import cats.Eval
import cats.free.Cofree
import java.nio.file.Path
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.Println
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType

sealed trait Symbol

object Symbol:

  // Templates ////////////////////////////////////////////////////////////////
  sealed trait Template extends Symbol
  case class Extension(field: Statement, body: List[Statement]) extends Template
  sealed abstract class CaseClass[T] extends Template:
    def nme: String
    def tpe: Term.Type[T]
    def fields: List[Statement]
    def body: List[Statement]

  object CaseClass:
    def apply[T](
      q: Option[Long],
      n: String,
      fs: List[Statement],
    ): CaseClass[T] =
      new CaseClass[T]:
        def nme = n
        def tpe: Term.Type[T] = Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(q, n, None)))
        def fields = fs
        def body = Nil

  // Object ///////////////////////////////////////////////////////////////////
  sealed abstract class Object[T] extends Symbol:
    def nme: String
    def par: Option[Term.Type[T]]
    def tpe: Term.Type[T]
    def body: List[Statement]

  object Object:
    def apply[T](
      q: Option[Long],
      n: String,
      p: Option[Term.Type[T]],
      b: List[Statement],
    ): Object[T] =
      new Object[T]:
        def nme = n
        def par = p
        def tpe: Term.Type[T] = Cofree((), Eval.now(
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

    type Type[T] = Cofree[[X] =>> TypeLevel[T, X], Unit]
    sealed trait TypeLevel[T, +X] extends Term
    object TypeLevel:
      type __
      case class App1[T[_], A, X](qnt: Option[Long], tfun: Type[T[__]], targ: Type[A]) extends TypeLevel[T[A], X]
      case class App2[T[_,_], A, B, X](qnt: Option[Long], tfun: Type[T[__,__]], ta: Type[A], tb: Type[B]) extends TypeLevel[T[A, B], X]
      case class App3[T[_,_,_], A, B, X](qnt: Option[Long], tfun: Type[T[__,__,__]], ta1: Type[A], ta2: Type[A], tb: Type[B]) extends TypeLevel[T[A, A, B], X]
      sealed abstract class Var[T, +X] extends TypeLevel[T, X]
      object Var:
        case class BooleanType[X](qnt: Option[Long]) extends Var[Boolean, X]
        case class IntType[X](qnt: Option[Long]) extends Var[Int, X]
        case class StringType[X](qnt: Option[Long]) extends Var[String, X]
        case class Function1Type[X](qnt: Option[Long]) extends Var[__ => __, X]
        case class Function2Type[X](qnt: Option[Long]) extends Var[(__, __) => __, X]
        case class ListType[G[_] <: List[?], X](qnt: Option[Long]) extends Var[G[__], X]
        case class OptionType[G[_] <: Option[?], X](qnt: Option[Long]) extends Var[G[__], X]
        object OptionType:
          case class SomeType[X](qnt: Option[Long]) extends Var[Option[__], X]
        case class UserDefinedType[T, X <: TypeLevel[T, X]](qnt: Option[Long], nme: String, impl: Option[Type[T]]) extends Var[T, X]
    
    type Value[T] = Cofree[[X] =>> ValueLevel[T, X], Unit]
   
    sealed trait ValueLevel[T, +X] extends Term:
      def tpe: Type[T]

    object ValueLevel:
      sealed abstract class App[T, X] extends Term.ValueLevel[T, X]
      object App:
        case class App1[A, B, X](qnt: Option[Long], fun: Value[A => B], arg: Value[A], tpe: Type[B]) extends Term.ValueLevel.App[B, X]
        case class AppCtor1[T, A, X](qnt: Option[Long], tpe: Type[T], arg: Value[A]) extends Term.ValueLevel.App[T, X]
        case class AppPure[G[_], A, X](qnt: Option[Long], fun: Value[A => G[A]], arg: Value[A], tpe: Type[G[A]]) extends Term.ValueLevel.App[G[A], X]
        case class AppVargs[A, B, X](qnt: Option[Long], fun: Value[List[A] => B], vargs: Value[A]*) extends Term.ValueLevel.App[B, X]:
          def tpe: Type[B] = ???
        case class Dot1[A, B, C, D, X](qnt: Option[Long], fun: Value[D], arg1: Value[A], arg2: Value[B]) extends Term.ValueLevel.App[C, X]:
          def tpe: Type[C] = ???
      sealed abstract class Lam[T, X] extends Term.ValueLevel[T, X]
      object Lam:
        case class Lam1[A, B, X](qnt: Option[Long], a: Value[A], b: Value[B]) extends Term.ValueLevel.Lam[A => B, X]:
          def tpe: Type[A => B] = Cofree((), Eval.now(Term.TypeLevel.App2(None, Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))), a.tail.value.tpe, b.tail.value.tpe)))
        case class Lam2[A, B, X](qnt: Option[Long], a1: Value[A], a2: Value[A], b: Value[B]) extends Term.ValueLevel.Lam[(A, A) => B, X]:
          def tpe: Type[(A, A) => B] = ???

      sealed abstract class Var[T, X] extends Term.ValueLevel[T, X]
      object Var:
        case class BooleanLiteral[X](qnt: Option[Long], b: Boolean) extends Var[Boolean, X]:
          def tpe: Type[Boolean] = Cofree((), Eval.now(Term.TypeLevel.Var.BooleanType(None)))
        case class IntLiteral[X](qnt: Option[Long], i: Int) extends Var[Int, X]:
          def tpe: Type[Int] = Cofree((), Eval.now(Term.TypeLevel.Var.IntType(None)))
        case class StringLiteral[X](qnt: Option[Long], s: String) extends Var[String, X]:
          def tpe: Type[String] = Cofree((), Eval.now(Term.TypeLevel.Var.StringType(None)))
        case class ListCtor[A, X](qnt: Option[Long]) extends Var[List[A] => List[A], X]:
          def tpe: Type[List[A] => List[A]] = ???
        case class OptionCtor[A, X](qnt: Option[Long]) extends Var[A => Option[A], X]:
          def tpe: Type[A => Option[A]] = ???
        object OptionCtor:
          case class SomeCtor[A, X](qnt: Option[Long]) extends Var[A => Some[A], X]:
            def tpe: Type[A => Some[A]] = Cofree((), Eval.now(Term.TypeLevel.App2(None, Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))), Cofree((), Eval.now(UserDefinedType(None, "A", None))), Cofree((), Eval.now(UserDefinedType(None, "A", None))))))
        case class Println[F[_], X](qnt: Option[Long], s: Value[String]) extends Var[F[Unit], X]:
          def tpe: Type[F[Unit]] = Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "cats.effect.IO[Unit]", None)))
        case class UserDefinedValue[T, X <: ValueLevel[T, X]](qnt: Option[Long], nme: String, tpe: Type[T], impl: Option[Value[T]]) extends Var[T, X]