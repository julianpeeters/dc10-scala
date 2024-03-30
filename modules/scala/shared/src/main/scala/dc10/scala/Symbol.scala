package dc10.scala

import java.nio.file.Path

sealed trait Symbol
object Symbol:

  // Templates ////////////////////////////////////////////////////////////////
  sealed trait Template extends Symbol
  case class Extension(field: Statement, body: List[Statement]) extends Template
  sealed abstract class CaseClass[T] extends Template:
    def nme: String
    def tpe: Term.TypeLevel[T]
    def fields: List[Statement]
    def body: List[Statement]

  object CaseClass:
    def apply[T](
      n: String,
      fs: List[Statement],
    ): CaseClass[T] =
      new CaseClass[T]:
        def nme = n
        def tpe: Term.TypeLevel[T] = Term.TypeLevel.Var.UserDefinedType(n, None)
        def fields = fs
        def body = Nil

  // Object ///////////////////////////////////////////////////////////////////
  sealed abstract class Object[T] extends Symbol:
    def nme: String
    def par: Option[Term.TypeLevel[T]]
    def tpe: Term.TypeLevel[T]
    def body: List[Statement]

  object Object:
    def apply[T](
      q: Option[Long],
      n: String,
      p: Option[Term.TypeLevel[T]],
      b: List[Statement],
    ): Object[T] =
      new Object[T]:
        def nme = n
        def par = p
        def tpe: Term.TypeLevel[T] =
          p.fold(
            Term.TypeLevel.Var.UserDefinedType(n, None)
          )(i => i)
        
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
  sealed trait Term extends Symbol

  object Term:

    sealed trait TypeLevel[T] extends Term
    object TypeLevel:

      sealed trait App[T] extends TypeLevel[T]
      object App:
        case class App1[T[_], A](tfun: TypeLevel[T[A]], targ: TypeLevel[A]) extends App[T[A]]
        case class App1T[T[_[_], _], F[_], A](tfun: TypeLevel[T[F, A]], farg: TypeLevel[F[A]], aarg: TypeLevel[A]) extends App[T[F, A]]
        case class App2[T[_,_], A, B](tfun: TypeLevel[T[A, B]], ta: TypeLevel[A], tb: TypeLevel[B]) extends App[T[A, B]]
        case class App2T[T[_[_],_,_], F[_], A, B, C](tfun: TypeLevel[T[F,A,B]], ta1: TypeLevel[F[C]], ta2: TypeLevel[A], tb: TypeLevel[B]) extends App[T[F, A, B]]
        case class App3[T[_,_,_], A, B, C](tfun: TypeLevel[T[A,B,C]], ta1: TypeLevel[A], ta2: TypeLevel[B], tb: TypeLevel[C]) extends App[T[A, B, C]]
        case class Infix[T[_,_], A, B](tfun: TypeLevel[T[A, B]], ta: TypeLevel[A], tb: TypeLevel[B]) extends App[T[A, B]]
      sealed trait Lam[T] extends TypeLevel[T]
      object Lam:
        case class Function1Type[A, B]() extends Lam[A => B]
        case class Function2Type[A, B, C]() extends Lam[(A, B) => C]
      sealed abstract class Var[T] extends TypeLevel[T]
      object Var:
        case class BooleanType() extends Var[Boolean]
        case class IntType() extends Var[Int]
        case class StringType() extends Var[String]
        case class UnitType() extends Var[Unit]
        case class ListType[A](a: TypeLevel[A]) extends Var[List[A]]
        case class OptionType[A](a: TypeLevel[A]) extends Var[Option[A]]
        case class SomeType[A](a: TypeLevel[A]) extends Var[Option[A]]
        case class TupleType[A, B](a: TypeLevel[A], b: TypeLevel[B]) extends Var[(A, B)]
        case class UserDefinedType[T](nme: String, impl: Option[TypeLevel[T]]) extends Var[T]
          
    sealed trait ValueLevel[T] extends Term
    object ValueLevel:

      sealed abstract class App[T] extends Term.ValueLevel[T]
      object App:
        case class App1[A, B](fun: ValueLevel[A => B], arg: ValueLevel[A], tpe: TypeLevel[B]) extends Term.ValueLevel.App[B]
        case class App2[A, B, C](fun: ValueLevel[(A, B) => C], arg: ValueLevel[A], arg2: ValueLevel[B], tpe: TypeLevel[C]) extends Term.ValueLevel.App[C]
        case class AppPure[G[_], A](fun: ValueLevel[G[A]], arg: ValueLevel[A], tpe: TypeLevel[G[A]]) extends Term.ValueLevel.App[G[A]]
        case class AppVargs[G[_], A](fun: ValueLevel[G[A]], tpe: TypeLevel[G[A]], vargs: ValueLevel[A]*) extends Term.ValueLevel.App[G[A]]
        case class Dot0[A, B, C](fun: ValueLevel[C], arg1: ValueLevel[A], tpe: TypeLevel[B]) extends Term.ValueLevel.App[B]
        case class Dot1[A, B, C, D](fun: ValueLevel[D], arg1: ValueLevel[A], arg2: ValueLevel[B], tpe: TypeLevel[C]) extends Term.ValueLevel.App[C]
        case class Dotless[A, B, C, D](fun: ValueLevel[D], arg1: ValueLevel[A], arg2: ValueLevel[B], tpe: TypeLevel[C]) extends Term.ValueLevel.App[C]
      sealed abstract class Blc[T] extends Term.ValueLevel[T]
      object Blc:
        case class ForComp[G[_], A](gens: List[Statement], ret: ValueLevel[A], tpe: TypeLevel[G[A]]) extends Blc[G[A]]
      sealed abstract class Lam[T] extends Term.ValueLevel[T]
      object Lam:
        case class Lam1[A, B](a: ValueLevel[A], b: ValueLevel[B], tpe: TypeLevel[A => B]) extends Term.ValueLevel.Lam[A => B]
        case class Lam2[A, B, C](a1: ValueLevel[A], a2: ValueLevel[B], c: ValueLevel[C], tpe: TypeLevel[(A, B) => C]) extends Term.ValueLevel.Lam[(A, B) => C]
      sealed abstract class Var[T] extends Term.ValueLevel[T]
      object Var:
        case class BooleanLiteral(tpe: TypeLevel[Boolean], b: Boolean) extends Var[Boolean]
        case class IntLiteral(tpe: TypeLevel[Int], i: Int) extends Var[Int]
        case class StringLiteral(tpe: TypeLevel[String], s: String) extends Var[String]
        case class UnitLiteral(tpe: TypeLevel[Unit], u: Unit) extends Var[Unit]
        case class UserDefinedValue[T](nme: String, tpe: TypeLevel[T], impl: Option[ValueLevel[T]]) extends Var[T]

      extension [T] (v: ValueLevel[T])
        def tpe: TypeLevel[T] =
          v match
            case Term.ValueLevel.App.App1(fun, arg, tpe) => tpe
            case Term.ValueLevel.App.App2(fun, arg1, arg2, tpe) => tpe 
            case Term.ValueLevel.App.AppPure(fun, arg, tpe) => tpe
            case Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => tpe
            case Term.ValueLevel.App.Dot0(fun, arg1, tpe) => tpe
            case Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.Blc.ForComp(l, r, tpe) => tpe
            case Term.ValueLevel.Lam.Lam1(a, b, tpe) => tpe
            case Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe) => tpe
            case Term.ValueLevel.Var.BooleanLiteral(tpe, b) => tpe
            case Term.ValueLevel.Var.IntLiteral(tpe, i) => tpe
            case Term.ValueLevel.Var.StringLiteral(tpe, s) => tpe
            case Term.ValueLevel.Var.UnitLiteral(tpe, u) => tpe
            case Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => tpe
            
    extension [T] (v: Term.ValueLevel[T])

      def findImpl: Option[Term.ValueLevel[T]] =
        v match
          case Term.ValueLevel.App.App1(fun, arg, tpe) => Some(v)
          case Term.ValueLevel.App.App2(fun, arg1, arg2, tpe) => Some(v) 
          case Term.ValueLevel.App.AppPure(fun, arg, tpe) => Some(v)
          case Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => Some(v)
          case Term.ValueLevel.App.Dot0(fun, arg1, tpe) => Some(v)
          case Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => Some(v)
          case Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => Some(v)
          case Term.ValueLevel.Blc.ForComp(l, r, t) => Some(v)
          case Term.ValueLevel.Lam.Lam1(a, b, t) => Some(v)
          case Term.ValueLevel.Lam.Lam2(a1, a2, b, t) => Some(v)
          case Term.ValueLevel.Var.BooleanLiteral(tpe, b) => Some(v)
          case Term.ValueLevel.Var.IntLiteral(tpe, i) => Some(v)
          case Term.ValueLevel.Var.StringLiteral(tpe, s) => Some(v)
          case Term.ValueLevel.Var.UnitLiteral(tpe, s) => Some(v)
          case u@Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => impl.fold(None)(i => i.findImpl)

      def findVargs[A]: Option[Seq[Term.ValueLevel[A]]] =
        v.findImpl.fold(None)(i => i match
          case Term.ValueLevel.App.App1(fun, arg, tpe) => None
          case Term.ValueLevel.App.App2(fun, arg1, arg2, tpe) => None
          case Term.ValueLevel.App.AppPure(fun, arg, tpe) => None
          case Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => Some(vargs.asInstanceOf[Seq[Term.ValueLevel[A]]])
          case Term.ValueLevel.App.Dot0(fun, arg1, tpe) => None
          case Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => None
          case Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => None
          case Term.ValueLevel.Blc.ForComp(l, r, t) => None
          case Term.ValueLevel.Lam.Lam1(a, b, t) => None
          case Term.ValueLevel.Lam.Lam2(a1, a2, b, t) => None
          case Term.ValueLevel.Var.BooleanLiteral(tpe, b) => None
          case Term.ValueLevel.Var.IntLiteral(tpe, i) => None
          case Term.ValueLevel.Var.StringLiteral(tpe, s) => None
          case Term.ValueLevel.Var.UnitLiteral(tpe, s) => None
          case Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => None
        )