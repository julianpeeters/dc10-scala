package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{LibraryDependency, TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term

trait PrimitiveTypes[F[_]]:
  type __
  def __ : F[TypeExpr[__]]

  def BOOLEAN: F[TypeExpr[Boolean]]
  given bLit: Conversion[Boolean, F[ValueExpr[Boolean]]]
  
  def INT: F[TypeExpr[Int]]
  given iLit: Conversion[Int, F[ValueExpr[Int]]]
  extension (fa: F[ValueExpr[Int]])
    def +:(fb: F[ValueExpr[Int]]): F[ValueExpr[Int]]
    
  def NOTHING: F[TypeExpr[Nothing]]

  def STRING: F[TypeExpr[String]]
  given sLit: Conversion[String, F[ValueExpr[String]]]

  def UNIT: F[TypeExpr[Unit]]
  given uLit: Conversion[Unit, F[ValueExpr[Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[[A] =>> StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]]:

    type __ = Term.TypeLevel.Var.__

    def __ : StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[__]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.__()))

    def BOOLEAN: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[Boolean]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.BooleanType()))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Boolean]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.BooleanLiteral(Term.TypeLevel.Var.BooleanType(), v)))

    def INT: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[Int]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.IntType()))

    given iLit: Conversion[Int, StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Int]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.IntLiteral(Term.TypeLevel.Var.IntType(), v)))

    extension (fa: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Int]])
      def +:(fb: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Int]]): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Int]] =
        for
          a <- fa
          b <- fb
          r <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Int]]((a.value, b.value) match
            case (Term.ValueLevel.App.App1(fun, arg, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.App2(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.AppPure(fun, arg, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.AppVargs(fun, tpe, vargs*), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dot0(fun, arg1, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Blc.ForComp(gens, ret, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Lam.Lam1(a, b, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.BooleanLiteral(tpe, b), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), Term.ValueLevel.Var.IntLiteral(tpe2, i2)) => Right(ValueExpr(Term.ValueLevel.Var.IntLiteral(tpe1, i1 + i2)))
            case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.StringLiteral(tpe, s), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.UnitLiteral(tpe, u), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.UserDefinedValue(nme, tpe, tparams, impl), _) => Left(List(Error("Not a value of Int")))
          )
        yield r
        
    def NOTHING: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[Nothing]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.NothingType()))

    def STRING: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[String]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.StringType()))
    
    given sLit: Conversion[String, StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[String]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.StringLiteral(Term.TypeLevel.Var.StringType(), v)))

    def UNIT: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UnitType()))
    
    given uLit: Conversion[Unit, StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.UnitLiteral(Term.TypeLevel.Var.UnitType(), v)))