package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term

trait PrimitiveTypes[F[_]]:

  def BOOLEAN: F[TypeExpr[Boolean]]
  given bLit: Conversion[Boolean, F[ValueExpr[Boolean]]]
  
  def INT: F[TypeExpr[Int]]
  given iLit: Conversion[Int, F[ValueExpr[Int]]]
  extension (fa: F[ValueExpr[Int]])
    def +:(fb: F[ValueExpr[Int]]): F[ValueExpr[Int]]
    
  def STRING: F[TypeExpr[String]]
  given sLit: Conversion[String, F[ValueExpr[String]]]

  def UNIT: F[TypeExpr[Unit]]
  given uLit: Conversion[Unit, F[ValueExpr[Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def BOOLEAN: StateT[ErrorF, List[Statement], TypeExpr[Boolean]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.BooleanType()))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, List[Statement], ValueExpr[Boolean]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.BooleanLiteral(Term.TypeLevel.Var.BooleanType(), v)))

    def INT: StateT[ErrorF, List[Statement], TypeExpr[Int]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.IntType()))

    given iLit: Conversion[Int, StateT[ErrorF, List[Statement], ValueExpr[Int]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.IntLiteral(Term.TypeLevel.Var.IntType(), v)))

    extension (fa: StateT[ErrorF, List[Statement], ValueExpr[Int]])
      def +:(fb: StateT[ErrorF, List[Statement], ValueExpr[Int]]): StateT[ErrorF, List[Statement], ValueExpr[Int]] =
        for
          a <- fa
          b <- fb
        yield (a.value, b.value) match
          case (Term.ValueLevel.App.App1(fun, arg, tpe), _) => ???
          case (Term.ValueLevel.App.App2(fun, arg1, arg2, tpe), _) => ???
          case (Term.ValueLevel.App.AppPure(fun, arg, tpe), _) => ???
          case (Term.ValueLevel.App.AppVargs(fun, tpe, vargs*), _) => ???
          case (Term.ValueLevel.App.Dot0(fun, arg1, tpe), _) => ???
          case (Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe), _) => ???
          case (Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe), _) => ???
          case (Term.ValueLevel.Blc.ForComp(gens, ret, tpe), _) => ???
          case (Term.ValueLevel.Lam.Lam1(a, b, tpe), _) => ???
          case (Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe), _) => ???
          case (Term.ValueLevel.Var.BooleanLiteral(tpe, b), _) => ???
          case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), Term.ValueLevel.Var.IntLiteral(tpe2, i2)) => ValueExpr(Term.ValueLevel.Var.IntLiteral(tpe1, i1 + i2))
          case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), _) => ???
          case (Term.ValueLevel.Var.StringLiteral(tpe, s), _) => ???
          case (Term.ValueLevel.Var.UnitLiteral(tpe, u), _) => ???
          case (Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl), _) => ???
        

    def STRING: StateT[ErrorF, List[Statement], TypeExpr[String]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.StringType()))
    
    given sLit: Conversion[String, StateT[ErrorF, List[Statement], ValueExpr[String]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.StringLiteral(Term.TypeLevel.Var.StringType(), v)))

    def UNIT: StateT[ErrorF, List[Statement], TypeExpr[Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UnitType()))
    
    given uLit: Conversion[Unit, StateT[ErrorF, List[Statement], ValueExpr[Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.UnitLiteral(Term.TypeLevel.Var.UnitType(), v)))