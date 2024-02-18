package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.TupleCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue

trait PrimitiveTypes[F[_]]:

  def BOOLEAN: F[TypeExpr[Boolean, Unit]]
  given bLit: Conversion[Boolean, F[ValueExpr[Boolean, Unit]]]
  
  def INT: F[TypeExpr[Int, Unit]]
  given iLit: Conversion[Int, F[ValueExpr[Int, Unit]]]
  extension (fa: F[ValueExpr[Int, Unit]])
    def +:(fb: F[ValueExpr[Int, Unit]]): F[ValueExpr[Int, Unit]]
    
  def STRING: F[TypeExpr[String, Unit]]
  given sLit: Conversion[String, F[ValueExpr[String, Unit]]]

  def UNIT: F[TypeExpr[Unit, Unit]]
  given uLit: Conversion[Unit, F[ValueExpr[Unit, Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def BOOLEAN: StateT[ErrorF, List[Statement], TypeExpr[Boolean, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.BooleanType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, List[Statement], ValueExpr[Boolean, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.BooleanLiteral(None, Term.TypeLevel.Var.BooleanType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), v)))

    def INT: StateT[ErrorF, List[Statement], TypeExpr[Int, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.IntType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))

    given iLit: Conversion[Int, StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.IntLiteral(None, Term.TypeLevel.Var.IntType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), v)))

    extension (fa: StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]])
      def +:(fb: StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]]): StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]] =
        for
          a <- fa
          b <- fb
        yield (a.value, b.value) match
          case (App1(qnt, fun, arg, tpe), _) => ???
          case (AppCtor1(qnt, tpe, arg), _) => ???
          case (AppCtor2(qnt, nme, tpe, arg1, arg2), _) => ???
          case (AppPure(qnt, fun, arg, tpe), _) => ???
          case (AppVargs(qnt, fun, tpe, vargs), _) => ???
          case (Dot1(qnt, fun, arg1, arg2, tpe), _) => ???
          case (Dotless(qnt, fun, arg1, arg2, tpe), _) => ???
          case (ForComp(qnt, gens, ret, tpe), _) => ???
          case (Lam1(qnt, a, b, tpe), _) => ???
          case (Lam2(qnt, a1, a2, c, tpe), _) => ???
          case (BooleanLiteral(qnt, tpe, b), _) => ???
          case (IntLiteral(qnt1, tpe1, i1), IntLiteral(qnt2, tpe2, i2)) => ValueExpr(IntLiteral(qnt1, tpe1, i1 + i2))
          case (IntLiteral(qnt1, tpe1, i1), _) => ???
          case (StringLiteral(qnt, tpe, s), _) => ???
          case (UnitLiteral(qnt, tpe, u), _) => ???
          case (ListCtor(qnt, tpe), _) => ???
          case (OptionCtor(qnt, tpe), _) => ???
          case (SomeCtor(qnt, tpe), _) => ???
          case (TupleCtor(qnt, tpe), _) => ???
          case (UserDefinedValue(qnt, nme, tpe, impl), _) => ???
        

    def STRING: StateT[ErrorF, List[Statement], TypeExpr[String, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.StringType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
    
    given sLit: Conversion[String, StateT[ErrorF, List[Statement], ValueExpr[String, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.StringLiteral(None, Term.TypeLevel.Var.StringType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), v)))

    def UNIT: StateT[ErrorF, List[Statement], TypeExpr[Unit, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UnitType(None)))
    
    given uLit: Conversion[Unit, StateT[ErrorF, List[Statement], ValueExpr[Unit, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), v)))