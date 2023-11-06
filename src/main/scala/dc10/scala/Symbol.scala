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
        def tpe: Term.TypeLevel[T, Unit] = Term.TypeLevel.Var.UserDefinedType(q, n, None, ())
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
            Term.TypeLevel.Var.UserDefinedType(q, n, None, ())
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

      extension [T, Z] (t: TypeLevel[T, Z])
        def dep: Z =
          t match
            case Symbol.Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => dep 
            case Symbol.Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => dep
            case Symbol.Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => dep
            case Symbol.Term.TypeLevel.Lam.Function1Type(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Lam.Function2Type(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.BooleanType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.IntType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.StringType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.ListType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.OptionType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.SomeType(qnt, dep) => dep
            case Symbol.Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => dep
            
      sealed trait App[T, Z] extends TypeLevel[T, Z]
      object App:
        case class App1[T[_], A, X, Y, Z](qnt: Option[Long], tfun: TypeLevel[T[A], X], targ: TypeLevel[A, Y], dep: Z) extends App[T[A], Z]
        case class App2[T[_,_], A, B, X, Z](qnt: Option[Long], tfun: TypeLevel[T[A, B], Z], ta: TypeLevel[A, Z], tb: TypeLevel[B, Z], dep: Z) extends App[T[A, B], Z]
        case class App3[T[_,_,_], A, B, C, X, Z](qnt: Option[Long], tfun: TypeLevel[T[A,B,C], Z], ta1: TypeLevel[A, Z], ta2: TypeLevel[B, Z], tb: TypeLevel[C, Z], dep: Z) extends App[T[A, B, C], Z]
      sealed trait Lam[T, Z] extends TypeLevel[T, Z]
      object Lam:
        case class Function1Type[A, B, Z](qnt: Option[Long], dep: Z) extends Lam[A => B, Z]
        case class Function2Type[A, B, C, Z](qnt: Option[Long], dep: Z) extends Lam[(A, B) => C, Z]
      sealed abstract class Var[T, Z] extends TypeLevel[T, Z]
      object Var:
        case class BooleanType[Z](qnt: Option[Long], dep: Z) extends Var[Boolean, Z]
        case class IntType[Z](qnt: Option[Long], dep: Z) extends Var[Int, Z]
        case class StringType[Z](qnt: Option[Long], dep: Z) extends Var[String, Z]
        case class ListType[A, Z](qnt: Option[Long], dep: Z) extends Var[A, Z]
        case class OptionType[A, Z](qnt: Option[Long], dep: Z) extends Var[A, Z]
        case class SomeType[A, Z](qnt: Option[Long], dep: Z) extends Var[A, Z]
        case class UserDefinedType[T, Z](qnt: Option[Long], nme: String, impl: Option[TypeLevel[T, Z]], dep: Z) extends Var[T, Z]
          
    sealed trait ValueLevel[T, Z] extends Term
    object ValueLevel:

      extension [T, Z] (v: ValueLevel[T, Z])
        def tpe: TypeLevel[T, Z] =
          v match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => tpe
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => tpe 
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => tpe
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => tpe
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => tpe
            case Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => tpe
            case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, tpe) => tpe
            case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => tpe
            case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => tpe
            case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => tpe
            case Term.ValueLevel.Var.ListCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => tpe
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => tpe
          
      sealed abstract class App[T, Z] extends Term.ValueLevel[T, Z]
      object App:
        case class App1[A, B, Z](qnt: Option[Long], fun: ValueLevel[A => B, Z], arg: ValueLevel[A, Z], tpe: TypeLevel[B, Z]) extends Term.ValueLevel.App[B, Z]
        case class AppCtor1[T, A, Z](qnt: Option[Long], tpe: TypeLevel[T, Z], arg: ValueLevel[A, Z]) extends Term.ValueLevel.App[T, Z]
        case class AppPure[G[_], A, X, Y, Z](qnt: Option[Long], fun: ValueLevel[G[A], X], arg: ValueLevel[A, Y], tpe: TypeLevel[G[A], Z]) extends Term.ValueLevel.App[G[A], Z]
        case class AppVargs[G[_], A, Y, Z](qnt: Option[Long], fun: ValueLevel[G[A], Y], tpe: TypeLevel[G[A], (Y, Z)], vargs: ValueLevel[A, Z]*) extends Term.ValueLevel.App[G[A], (Y, Z)]
        case class Dot1[A, B, C, D, Z](qnt: Option[Long], fun: ValueLevel[D, Z], arg1: ValueLevel[A, Z], arg2: ValueLevel[B, Z], tpe: TypeLevel[C, Z]) extends Term.ValueLevel.App[C, Z]
        case class Dotless[A, B, C, D, Z](qnt: Option[Long], fun: ValueLevel[D, Z], arg1: ValueLevel[A, Z], arg2: ValueLevel[B, Z], tpe: TypeLevel[C, Z]) extends Term.ValueLevel.App[C, Z]
      sealed abstract class Lam[T, Z] extends Term.ValueLevel[T, Z]
      object Lam:
        case class Lam1[A, B, Z](qnt: Option[Long], a: ValueLevel[A, Z], b: ValueLevel[B, Z], tpe: TypeLevel[A => B, Z]) extends Term.ValueLevel.Lam[A => B, Z]
        case class Lam2[A, B, Z](qnt: Option[Long], a1: ValueLevel[A, Z], a2: ValueLevel[A, Z], b: ValueLevel[B, Z], tpe: TypeLevel[(A, A) => B, Z]) extends Term.ValueLevel.Lam[(A, A) => B, Z]
      sealed abstract class Var[T, Z] extends Term.ValueLevel[T, Z]
      object Var:
        case class BooleanLiteral[Z](qnt: Option[Long], tpe: TypeLevel[Boolean, Z], b: Boolean) extends Var[Boolean, Z]
        case class IntLiteral[Z](qnt: Option[Long], tpe: TypeLevel[Int, Z], i: Int) extends Var[Int, Z]
        case class StringLiteral[Z](qnt: Option[Long], tpe: TypeLevel[String, Z], s: String) extends Var[String, Z]
        case class ListCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class OptionCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class SomeCtor[A, Z](qnt: Option[Long], tpe: TypeLevel[A, Z]) extends Var[A, Z]
        case class UserDefinedValue[T, Z](qnt: Option[Long], nme: String, tpe: TypeLevel[T, Z], impl: Option[ValueLevel[T, Z]]) extends Var[T, Z]
              