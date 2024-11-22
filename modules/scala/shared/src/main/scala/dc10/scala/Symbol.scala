package dc10.scala

import dc10.scala.Symbol.Term.TypeLevel

sealed trait Symbol
object Symbol:

  case class CaseClass[T](nme: String, fields: List[Statement], body: List[Statement]) extends Symbol
  case class Extension(field: Statement, body: List[Statement]) extends Symbol

  sealed trait Package extends Symbol
  object Package:
    case class Basic(nme: String, nst: Statement.`package`) extends Package
    case class Empty(ms : List[Statement]) extends Package

  sealed trait Trait extends Symbol
  object Trait:
    case class `*`[T](nme: String, parent: Option[Term.TypeLevel], body: List[Statement]) extends Trait
    case class `*->*`[F[_]](nme: String, parent: Option[Term.TypeLevel], body: List[Statement]) extends Trait
    case class `(*->*)->*`[F[_[_]]](nme: String, parent: Option[Term.TypeLevel], body: List[Statement]) extends Trait
    case class `(*->*)->*->*`[F[_[_], _]](nme: String, parent: Option[Term.TypeLevel], body: List[Statement]) extends Trait
  
  sealed trait Term extends Symbol
  object Term:

    sealed trait TypeLevel extends Term
    object TypeLevel:
      
      type __
      sealed trait `*`[+T] extends TypeLevel
      sealed trait `*->*`[T[_]] extends TypeLevel
      sealed trait `*->*->*`[T[_, _]] extends TypeLevel
      sealed trait `*->*->*->*`[T[_, _, _]] extends TypeLevel
      sealed trait `(*->*)->*`[T[_[_]]] extends TypeLevel
      sealed trait `(*->*)->*->*`[T[_[_], _]] extends TypeLevel
      sealed trait `(*->*)->*->*->*`[T[_[_], _, _]] extends TypeLevel
      sealed trait `((*->*)->*->*)->*`[T[_[_[_], _]]] extends TypeLevel
      
      object App:
        case class `App[_]`[T[_], A](tfun: `*->*`[T], aarg: `*`[A]) extends `*`[T[A]]
        case class `App[_[_]]`[T[_[_]], F[_]](tfun: `(*->*)->*`[T], farg: `*->*`[F]) extends `*`[T[F]]
        case class `App[_, _]`[T[_,_], A, B](tfun: `*->*->*`[T], ta: `*`[A], tb: `*`[B]) extends `*`[T[A, B]]
        case class `App[_[_], _]`[T[_[_], _], F[_], A](tfun: `(*->*)->*->*`[T], farg: `*->*`[F], aarg: `*`[A]) extends `*`[T[F, A]]
        case class `App[_, _, _]`[T[_,_,_], A, B, C](tfun: `*->*->*->*`[T], ta1: `*`[A], ta2: `*`[B], tb: `*`[C]) extends `*`[T[A, B, C]]
        case class `App[_[_], _, _]`[T[_[_], _, _], F[_], A, B](tfun: `(*->*)->*->*->*`[T], farg: `*->*`[F], aarg: `*`[A], barg: `*`[B]) extends `*`[T[F, A, B]]
        case class `App[_[_[_], _]]`[T[_[_[_], _]], F[_[_], _]](tfun: `((*->*)->*->*)->*`[T], arg: `(*->*)->*->*`[F]) extends `*`[T[F]]
        case class Infix[T[_,_], A, B](tfun: `*->*->*`[T], ta: `*`[A], tb: `*`[B]) extends `*`[T[A, B]]
        case class Infix2[T[_,_,_], A, B, C](tfun: `*->*->*->*`[T], ta: `*`[A], tb: `*`[B], tc: `*`[C]) extends `*`[T[A, B, C]]
      object Lam:
        case class Lam[F[_], A](domain: `*`[A], codomain: `*`[F[A]]) extends `*->*`[F]
      object Var:
        case class `UserDefinedType`[T](nme: String, impl: Option[`*`[T]]) extends `*`[T]
        case class `UserDefinedType[_]`[T[_]](nme: String, impl: Option[`*->*`[T]]) extends `*->*`[T]
        case class `UserDefinedType[_[_]]`[T[_[_]]](nme: String, impl: Option[`(*->*)->*`[T]]) extends `(*->*)->*`[T]
        case class `UserDefinedType[_, _]`[T[_, _]](nme: String, impl: Option[`*->*->*`[T]]) extends `*->*->*`[T]
        case class `UserDefinedType[_[_], _]`[T[_[_], _]](nme: String, impl: Option[`(*->*)->*->*`[T]]) extends `(*->*)->*->*`[T]
        case class `UserDefinedType[_, _, _]`[T[_, _, _]](nme: String, impl: Option[`*->*->*->*`[T]]) extends `*->*->*->*`[T]
        case class `UserDefinedType[_[_], _, _]`[T[_[_], _, _]](nme: String, impl: Option[`(*->*)->*->*->*`[T]]) extends `(*->*)->*->*->*`[T]
        case class `UserDefinedType[_[_[_], _]]`[T[_[_[_], _]]](nme: String, impl: Option[`((*->*)->*->*)->*`[T]]) extends `((*->*)->*->*)->*`[T]

    sealed trait ValueLevel extends Term
    object ValueLevel:

      sealed trait `*`[T] extends ValueLevel
      sealed trait `*->*`[T[_]] extends ValueLevel
      sealed trait `(*->*)->*->*`[T[_[_], _]] extends ValueLevel

      object App:
        case class App1[A, B](fun: ValueLevel.`*`[A => B], arg: ValueLevel.`*`[A], tpe: TypeLevel.`*`[B]) extends `*`[B]
        case class App2[A, B, C](fun: ValueLevel.`*`[(A, B) => C], arg: ValueLevel.`*`[A], arg2: ValueLevel.`*`[B], tpe: TypeLevel.`*`[C]) extends `*`[C]
        case class AppPure[G[_], A](fun: ValueLevel.`*`[G[A]], arg: ValueLevel.`*`[A], tpe: TypeLevel.`*`[G[A]]) extends `*`[G[A]]
        case class AppVargs[G[_], A](fun: ValueLevel.`*`[G[A]], tpe: TypeLevel.`*`[G[A]], vargs: ValueLevel.`*`[A]*) extends `*`[G[A]]
        case class Dot0[A, B, C](fun: ValueLevel.`*`[C], arg1: ValueLevel.`*`[A], tpe: TypeLevel.`*`[B]) extends `*`[B]
        case class Dot1[A, B, C, D](fun: ValueLevel.`*`[D], arg1: ValueLevel.`*`[A], arg2: ValueLevel.`*`[B], tpe: TypeLevel.`*`[C]) extends `*`[C]
        case class Dotless[A, B, C, D](fun: ValueLevel.`*`[D], arg1: ValueLevel.`*`[A], arg2: ValueLevel.`*`[B], tpe: TypeLevel.`*`[C]) extends `*`[C]
        case class ForComp[G[_], A](gens: List[Statement], ret: ValueLevel.`*`[A], tpe: TypeLevel.`*`[G[A]]) extends `*`[G[A]]
      object Lam:
        case class Lam1[A, B](a: ValueLevel.`*`[A], b: ValueLevel.`*`[B], tpe: TypeLevel.`*`[A => B]) extends `*`[A => B]
        case class Lam2[A, B, C](a1: ValueLevel.`*`[A], a2: ValueLevel.`*`[B], c: ValueLevel.`*`[C], tpe: TypeLevel.`*`[(A, B) => C]) extends `*`[(A, B) => C]
      object Var:
        case class BooleanLiteral(tpe: TypeLevel.`*`[Boolean], b: Boolean) extends `*`[Boolean]
        case class IntLiteral(tpe: TypeLevel.`*`[Int], i: Int) extends `*`[Int]
        case class StringLiteral(tpe: TypeLevel.`*`[String], s: String) extends `*`[String]
        case class UnitLiteral(tpe: TypeLevel.`*`[Unit], u: Unit) extends `*`[Unit]
        case class `UserDefinedObject`[T](nme: String, tpe: TypeLevel.`*`[T], parent: Option[TypeLevel.`*`[T]], body: List[Statement]) extends `*`[T]
        case class `UserDefinedValue`[T](nme: String, tpe: TypeLevel.`*`[T], impl: Option[ValueLevel.`*`[T]]) extends `*`[T]
        case class `UserDefinedValue[_]`[T[_]](nme: String, tpe: TypeLevel.`*->*`[T], impl: Option[ValueLevel.`*->*`[T]]) extends `*->*`[T]
        case class `UserDefinedValue[_[_], _]`[T[_[_], _]](nme: String, tpe: TypeLevel.`(*->*)->*->*`[T], impl: Option[ValueLevel.`(*->*)->*->*`[T]]) extends `(*->*)->*->*`[T]

      extension [T] (v: ValueLevel.`*`[T])
        def tpe: TypeLevel.`*`[T] =
          v match
            case ValueLevel.App.App1(fun, arg, tpe) => tpe
            case ValueLevel.App.App2(fun, arg1, arg2, tpe) => tpe 
            case ValueLevel.App.AppPure(fun, arg, tpe) => tpe
            case ValueLevel.App.AppVargs(fun, tpe, vargs*) => tpe
            case ValueLevel.App.Dot0(fun, arg1, tpe) => tpe
            case ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => tpe
            case ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => tpe
            case ValueLevel.App.ForComp(l, r, tpe) => tpe
            case ValueLevel.Lam.Lam1(a, b, tpe) => tpe
            case ValueLevel.Lam.Lam2(a1, a2, c, tpe) => tpe
            case ValueLevel.Var.BooleanLiteral(tpe, b) => tpe
            case ValueLevel.Var.IntLiteral(tpe, i) => tpe
            case ValueLevel.Var.StringLiteral(tpe, s) => tpe
            case ValueLevel.Var.UnitLiteral(tpe, u) => tpe
            case ValueLevel.Var.UserDefinedObject(nme, tpe, parent, impl) => parent.fold(tpe)(identity)
            case ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => tpe
            
    extension [T] (v: ValueLevel.`*`[T])
      def findImpl: Option[ValueLevel.`*`[T]] =
        v match
          case ValueLevel.App.App1(fun, arg, tpe) => Some(v)
          case ValueLevel.App.App2(fun, arg1, arg2, tpe) => Some(v) 
          case ValueLevel.App.AppPure(fun, arg, tpe) => Some(v)
          case ValueLevel.App.AppVargs(fun, tpe, vargs*) => Some(v)
          case ValueLevel.App.Dot0(fun, arg1, tpe) => Some(v)
          case ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => Some(v)
          case ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => Some(v)
          case ValueLevel.App.ForComp(l, r, t) => Some(v)
          case ValueLevel.Lam.Lam1(a, b, t) => Some(v)
          case ValueLevel.Lam.Lam2(a1, a2, b, t) => Some(v)
          case ValueLevel.Var.BooleanLiteral(tpe, b) => Some(v)
          case ValueLevel.Var.IntLiteral(tpe, i) => Some(v)
          case ValueLevel.Var.StringLiteral(tpe, s) => Some(v)
          case ValueLevel.Var.UnitLiteral(tpe, s) => Some(v)
          case ValueLevel.Var.UserDefinedObject(nme, tpe, parent, impl) => Some(v)
          case ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => impl.fold(None)(i => i.findImpl)