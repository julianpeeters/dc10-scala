package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel

trait PrimitiveTypes[F[_]]:

  def BOOLEAN: F[TypeExpr[Boolean, Unit]]
  given bLit: Conversion[Boolean, F[ValueExpr[Boolean, Unit]]]
  
  def INT: F[TypeExpr[Int, Unit]]
  given iLit: Conversion[Int, F[ValueExpr[Int, Unit]]]
  
  def STRING: F[TypeExpr[String, Unit]]
  given sLit: Conversion[String, F[ValueExpr[String, Unit]]]

  def UNIT: F[TypeExpr[Unit, Unit]]
  given uLit: Conversion[Unit, F[ValueExpr[Unit, Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def BOOLEAN: StateT[ErrorF, List[Statement], TypeExpr[Boolean, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.BooleanType(None, ())))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, List[Statement], ValueExpr[Boolean, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.BooleanLiteral(None, Term.TypeLevel.Var.BooleanType(None, ()), v)))

    def INT: StateT[ErrorF, List[Statement], TypeExpr[Int, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.IntType(None, ())))

    given iLit: Conversion[Int, StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.IntLiteral(None, Term.TypeLevel.Var.IntType(None, ()), v)))

    def STRING: StateT[ErrorF, List[Statement], TypeExpr[String, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.StringType(None, ())))
    
    given sLit: Conversion[String, StateT[ErrorF, List[Statement], ValueExpr[String, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.StringLiteral(None, Term.TypeLevel.Var.StringType(None, ()), v)))

    def UNIT: StateT[ErrorF, List[Statement], TypeExpr[Unit, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UnitType(None, ())))
    
    given uLit: Conversion[Unit, StateT[ErrorF, List[Statement], ValueExpr[Unit, Unit]]] =
      v => StateT.pure(ValueExpr(Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None, ()), v)))