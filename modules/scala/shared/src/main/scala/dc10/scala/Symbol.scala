package dc10.scala

import java.nio.file.Path

sealed trait Symbol
object Symbol:

  // Templates ////////////////////////////////////////////////////////////////
  sealed trait Template extends Symbol
  case class Extension(field: Statement, body: List[Statement]) extends Template
  sealed abstract class CaseClass[T, Z] extends Template:
    def nme: String
    def tpe: Term.TypeLevel[T, Z]
    def fields: List[Statement]
    def body: List[Statement]

  object CaseClass:
    def apply[T](
      q: Option[Long],
      n: String,
      fs: List[Statement],
    ): CaseClass[T, Unit] =
      new CaseClass[T, Unit]:
        def nme = n
        def tpe: Term.TypeLevel[T, Unit] = Term.TypeLevel.Var.UserDefinedType(q, n, None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))
        def fields = fs
        def body = Nil

  // Object ///////////////////////////////////////////////////////////////////
  sealed abstract class Object[T, Z] extends Symbol:
    def nme: String
    def par: Option[Term.TypeLevel[T, Z]]
    def tpe: Term.TypeLevel[T, Z]
    def body: List[Statement]

  object Object:
    def apply[T](
      q: Option[Long],
      n: String,
      p: Option[Term.TypeLevel[T, Unit]],
      b: List[Statement],
    ): Object[T, Unit] =
      new Object[T, Unit]:
        def nme = n
        def par = p
        def tpe: Term.TypeLevel[T, Unit] = p.fold(
            Term.TypeLevel.Var.UserDefinedType(q, n, None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))
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

    sealed trait TypeLevel[T, Z] extends Term
    object TypeLevel:

      sealed trait App[T, Z] extends TypeLevel[T, Z]
      object App:
        case class App1[T[_], A, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[A], X], targ: TypeLevel[A, Y], dep: ValueLevel[Z, Any]) extends App[T[A], Z]
        case class App1T[T[_[_], _], F[_], A, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[F, A], X], farg: TypeLevel[F[A], Y], aarg: TypeLevel[A, Y],  dep: ValueLevel[Z, Any]) extends App[T[F, A], Z]
        case class App2[T[_,_], A, B, Q, W, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[A, B], W], ta: TypeLevel[A, X], tb: TypeLevel[B, Y], dep: ValueLevel[Z, Any]) extends App[T[A, B], Z]
        case class App2T[T[_[_],_,_], F[_], A, B, C, Y, X, Z](qnt: Option[Long], tfun: TypeLevel[T[F,A,B], Y], ta1: TypeLevel[F[C], Z], ta2: TypeLevel[A, Z], tb: TypeLevel[B, Z], dep: ValueLevel[Z, Any]) extends App[T[F, A, B], Z]
        case class App3[T[_,_,_], A, B, C, X, Z](qnt: Option[Long], tfun: TypeLevel[T[A,B,C], Z], ta1: TypeLevel[A, Z], ta2: TypeLevel[B, Z], tb: TypeLevel[C, Z], dep: ValueLevel[Z, Any]) extends App[T[A, B, C], Z]
        case class Infix[T[_,_], A, B, W, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[A, B], W], ta: TypeLevel[A, X], tb: TypeLevel[B, Y], dep: ValueLevel[Z, Any]) extends App[T[A, B], Z]
        case class InfixPi[T[_,_], A, B, W, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[A, B], W], a: ValueLevel[A, X], tb: TypeLevel[B, Y], dep: ValueLevel[Z, Any]) extends App[T[A, B], Z]
      sealed trait Lam[T, Z] extends TypeLevel[T, Z]
      object Lam:
        case class Function1Type[A, B, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Lam[A => B, Z]
        case class Function2Type[A, B, C, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Lam[(A, B) => C, Z]
      sealed abstract class Var[T, Z] extends TypeLevel[T, Z]
      object Var:
        case class BooleanType[Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[Boolean, Z]
        case class IntType[Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[Int, Z]
        case class StringType[Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[String, Z]
        case class UnitType[Z](qnt: Option[Long]) extends Var[Unit, Z]
        case class ListType[A, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[A, Z]
        case class OptionType[A, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[A, Z]
        case class SomeType[A, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[A, Z]
        case class TupleType[A, Z](qnt: Option[Long], dep: ValueLevel[Z, Any]) extends Var[A, Z]
        case class UserDefinedType[T, Z](qnt: Option[Long], nme: String, impl: Option[TypeLevel[T, Z]], dep: ValueLevel[Z, Any]) extends Var[T, Z]
          
    sealed trait ValueLevel[T, Z] extends Term
    object ValueLevel:

      sealed abstract class App[T, Z] extends Term.ValueLevel[T, Z]
      object App:
        case class App1[A, B, Z](qnt: Option[Long], fun: ValueLevel[A => B, Z], arg: ValueLevel[A, Z], tpe: TypeLevel[B, Z]) extends Term.ValueLevel.App[B, Z]
        case class AppCtor1[T, A, Z](qnt: Option[Long], tpe: TypeLevel[T, Z], arg: ValueLevel[A, Z]) extends Term.ValueLevel.App[T, Z]
        case class AppCtor2[T, A, B, X, Y, Z](qnt: Option[Long], nme: String, tpe: TypeLevel[T, Z], arg1: ValueLevel[A, X], arg2: ValueLevel[B, Y]) extends Term.ValueLevel.App[T, Z]
        case class AppPure[G[_], A, X, Y, Z](qnt: Option[Long], fun: ValueLevel[G[A], X], arg: ValueLevel[A, Y], tpe: TypeLevel[G[A], Z]) extends Term.ValueLevel.App[G[A], Z]
        case class AppVargs[G[_], A, Y, Z](qnt: Option[Long], fun: ValueLevel[G[A], Y], tpe: TypeLevel[G[A], (Y, Z)], vargs: ValueLevel[A, Z]*) extends Term.ValueLevel.App[G[A], (Y, Z)]
        case class Dot1[A, B, C, D, Z](qnt: Option[Long], fun: ValueLevel[D, Z], arg1: ValueLevel[A, Z], arg2: ValueLevel[B, Z], tpe: TypeLevel[C, Z]) extends Term.ValueLevel.App[C, Z]
        case class Dotless[A, B, C, D, Z](qnt: Option[Long], fun: ValueLevel[D, Z], arg1: ValueLevel[A, Z], arg2: ValueLevel[B, Z], tpe: TypeLevel[C, Z]) extends Term.ValueLevel.App[C, Z]
      sealed abstract class Blc[T, Z] extends Term.ValueLevel[T, Z]
      object Blc:
        case class ForComp[G[_], A, X, Z](qnt: Option[Long], gens: List[Statement], ret: ValueLevel[A, X], tpe: TypeLevel[G[A], Z]) extends Blc[G[A], Z]
      sealed abstract class Lam[T, Z] extends Term.ValueLevel[T, Z]
      object Lam:
        case class Lam1[A, B, X, Y, Z](qnt: Option[Long], a: ValueLevel[A, X], b: ValueLevel[B, Y], tpe: TypeLevel[A => B, Z]) extends Term.ValueLevel.Lam[A => B, Z]
        case class Lam2[A, B, C, Z](qnt: Option[Long], a1: ValueLevel[A, Z], a2: ValueLevel[B, Z], c: ValueLevel[C, Z], tpe: TypeLevel[(A, B) => C, Z]) extends Term.ValueLevel.Lam[(A, B) => C, Z]
      sealed abstract class Var[T, Z] extends Term.ValueLevel[T, Z]
      object Var:
        case class BooleanLiteral[Z](qnt: Option[Long], tpe: TypeLevel[Boolean, Z], b: Boolean) extends Var[Boolean, Z]
        case class IntLiteral[Z](qnt: Option[Long], tpe: TypeLevel[Int, Z], i: Int) extends Var[Int, Z]
        case class StringLiteral[Z](qnt: Option[Long], tpe: TypeLevel[String, Z], s: String) extends Var[String, Z]
        case class UnitLiteral[Z](qnt: Option[Long], tpe: TypeLevel[Unit, Z], u: Unit) extends Var[Unit, Z]
        case class ListCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class OptionCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class SomeCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class TupleCtor[A, B, Z](qnt: Option[Long], tpe: TypeLevel[Tuple2[A, B], Z]) extends Var[(A, B), Z]
        case class UserDefinedValue[T, Z](qnt: Option[Long], nme: String, tpe: TypeLevel[T, Z], impl: Option[ValueLevel[T, Z]]) extends Var[T, Z]

      extension [T, Z] (v: ValueLevel[T, Z])
        def tpe: TypeLevel[T, Z] =
          v match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => tpe
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => tpe 
            case Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => tpe 
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => tpe
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => tpe
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.Blc.ForComp(qnt, l, r, tpe) => tpe
            case Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => tpe
            case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, c, tpe) => tpe
            case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => tpe
            case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => tpe
            case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => tpe
            case Term.ValueLevel.Var.UnitLiteral(qnt, tpe, u) => tpe
            case Term.ValueLevel.Var.ListCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.TupleCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => tpe

    extension [T, Z] (t: TypeLevel[T, Z])
      def dep: ValueLevel[Z, Any] =
        t match
          case Symbol.Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => dep
          case Symbol.Term.TypeLevel.App.App1T(qnt, tfun, farg, aarg, dep) => dep
          case Symbol.Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => dep
          case Symbol.Term.TypeLevel.App.App2T(qnt, tfun, ta1, ta2, tb, dep) => dep
          case Symbol.Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => dep
          case Symbol.Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => dep
          case Symbol.Term.TypeLevel.App.InfixPi(qnt, tfun, a, tb, dep) => dep
          case Symbol.Term.TypeLevel.Lam.Function1Type(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Lam.Function2Type(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.BooleanType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.IntType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.StringType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.UnitType(qnt) => Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()).asInstanceOf[ValueLevel[Z, Any]]
          case Symbol.Term.TypeLevel.Var.ListType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.OptionType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.SomeType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.TupleType(qnt, dep) => dep
          case Symbol.Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => dep
            

    extension [T, Z] (t: Term.TypeLevel[T, Z])
      def manageDep[ZZ](f: ValueLevel[Z, Any] => ValueLevel[ZZ, Any]): Term.TypeLevel[T, ZZ] =
        t match
          case Term.TypeLevel.App.App1(qnt, tfun, targ, dep) =>  Term.TypeLevel.App.App1(qnt, tfun, targ, f(dep))
          case Term.TypeLevel.App.App1T(qnt, tfun, farg, aarg, dep) => Term.TypeLevel.App.App1T(qnt, tfun, farg, aarg, f(dep))
          case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => Term.TypeLevel.App.App2(qnt, tfun, ta, tb, f(dep))
          case Term.TypeLevel.App.App2T(qnt, tfun, ta1, ta2, tb, dep) => Term.TypeLevel.App.App2T(qnt, tfun, ta1.manageDep(f), ta2.manageDep(f), tb.manageDep(f), f(dep))
          case Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => Term.TypeLevel.App.App3(qnt, tfun.manageDep(f), ta1.manageDep(f), ta2.manageDep(f), tb.manageDep(f), f(dep))
          case Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, f(dep))
          case Term.TypeLevel.App.InfixPi(qnt, tfun, a, tb, dep) => Term.TypeLevel.App.InfixPi(qnt, tfun, a, tb, f(dep))
          case Term.TypeLevel.Lam.Function1Type(qnt, dep) => Term.TypeLevel.Lam.Function1Type(qnt, f(dep)).asInstanceOf[Term.TypeLevel[T, ZZ]]
          case Term.TypeLevel.Lam.Function2Type(qnt, dep) => Term.TypeLevel.Lam.Function2Type(qnt, f(dep)).asInstanceOf[Term.TypeLevel[T, ZZ]]
          case Term.TypeLevel.Var.BooleanType(qnt, dep) => Term.TypeLevel.Var.BooleanType(qnt, f(dep))
          case Term.TypeLevel.Var.IntType(qnt, dep) => Term.TypeLevel.Var.IntType(qnt, f(dep))
          case Term.TypeLevel.Var.StringType(qnt, dep) => Term.TypeLevel.Var.StringType(qnt, f(dep))
          case Term.TypeLevel.Var.UnitType(qnt) => Term.TypeLevel.Var.UnitType(qnt)
          case Term.TypeLevel.Var.ListType(qnt, dep) => Term.TypeLevel.Var.ListType(qnt, f(dep))
          case Term.TypeLevel.Var.OptionType(qnt, dep) => Term.TypeLevel.Var.OptionType(qnt, f(dep))
          case Term.TypeLevel.Var.SomeType(qnt, dep) => Term.TypeLevel.Var.SomeType(qnt, f(dep))
          case Term.TypeLevel.Var.TupleType(qnt, dep) => Term.TypeLevel.Var.TupleType(qnt, f(dep))
          case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl.map(i => i.manageDep(f)), f(dep))

    extension [T, Z] (v: Term.ValueLevel[T, Z])
      def manageDep[ZZ](f: ValueLevel[Z, Any] => ValueLevel[ZZ, Any]): Term.ValueLevel[T, ZZ] =
        v match
          case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => Term.ValueLevel.App.App1(qnt, fun.manageDep(f), arg.manageDep(f), tpe.manageDep(f))
          case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => Term.ValueLevel.App.AppCtor1(qnt, tpe.manageDep(f), arg.manageDep(f))
          case Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => Term.ValueLevel.App.AppCtor2(qnt, nme, tpe.manageDep(f), arg1, arg2)
          case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe.manageDep(f))
          case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => ???
          case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => Term.ValueLevel.App.Dot1(qnt, fun.manageDep(f), arg1.manageDep(f), arg2.manageDep(f), tpe.manageDep(f))
          case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => Term.ValueLevel.App.Dotless(qnt, fun.manageDep(f), arg1.manageDep(f), arg2.manageDep(f), tpe.manageDep(f))
          case Term.ValueLevel.Blc.ForComp(qnt, l, r, tpe) => Term.ValueLevel.Blc.ForComp(qnt, l, r, tpe.manageDep(f))
          case Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe.manageDep(f))
          case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, tpe) => ???
          case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => Term.ValueLevel.Var.BooleanLiteral(qnt, tpe.manageDep(f), b)
          case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => Term.ValueLevel.Var.IntLiteral(qnt, tpe.manageDep(f), i)
          case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => Term.ValueLevel.Var.StringLiteral(qnt, tpe.manageDep(f), s)
          case Term.ValueLevel.Var.UnitLiteral(qnt, tpe, s) => Term.ValueLevel.Var.UnitLiteral(qnt, tpe.manageDep(f), s)
          case Term.ValueLevel.Var.ListCtor(qnt, tpe) => Term.ValueLevel.Var.ListCtor(qnt, tpe.manageDep(f))
          case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => Term.ValueLevel.Var.OptionCtor(qnt, tpe.manageDep(f))
          case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => Term.ValueLevel.Var.SomeCtor(qnt, tpe.manageDep(f))
          case Term.ValueLevel.Var.TupleCtor(qnt, tpe) => Term.ValueLevel.Var.TupleCtor(qnt, tpe.manageDep(f))
          case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe.manageDep(f), impl.map(i => i.manageDep(f)))

      def findImpl: Option[Term.ValueLevel[T, Z]] =
        v match
          case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => Some(v)
          case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => Some(v)
          case Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => Some(v)
          case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Some(v)
          case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => Some(v)
          case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => Some(v)
          case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => Some(v)
          case Term.ValueLevel.Blc.ForComp(qnt, l, r, t) => Some(v)
          case Term.ValueLevel.Lam.Lam1(qnt, a, b, t) => Some(v)
          case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, t) => Some(v)
          case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => Some(v)
          case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => Some(v)
          case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => Some(v)
          case Term.ValueLevel.Var.UnitLiteral(qnt, tpe, s) => Some(v)
          case Term.ValueLevel.Var.ListCtor(qnt, tpe) => Some(v)
          case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => Some(v)
          case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => Some(v)
          case Term.ValueLevel.Var.TupleCtor(qnt, tpe) => Some(v)
          case u@Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => impl.fold(None)(i => i.findImpl)

      def findVargs[U, A]: Option[Seq[Term.ValueLevel[U, A]]] =
        v.findImpl.fold(None)(i => i match
          case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => None
          case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => None
          case Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => None
          case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => None
          case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => Some(vargs.asInstanceOf[Seq[Term.ValueLevel[U, A]]])
          case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => None
          case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => None
          case Term.ValueLevel.Blc.ForComp(qnt, l, r, t) => None
          case Term.ValueLevel.Lam.Lam1(qnt, a, b, t) => None
          case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, t) => None
          case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => None
          case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => None
          case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => None
          case Term.ValueLevel.Var.UnitLiteral(qnt, tpe, s) => None
          case Term.ValueLevel.Var.ListCtor(qnt, tpe) => None
          case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => None
          case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => None
          case Term.ValueLevel.Var.TupleCtor(qnt, tpe) => None
          case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => None
        )
