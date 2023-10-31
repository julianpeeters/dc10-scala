package dc10.scala.predef.datatype

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel

trait PrimitiveTypes[F[_]]:

  def BOOLEAN: F[TypeExpr[Unit, Boolean]]
  given bLit: Conversion[Boolean, F[ValueExpr[Unit, Boolean]]]
  
  def INT: F[TypeExpr[Unit, Int]]
  given iLit: Conversion[Int, F[ValueExpr[Unit, Int]]]
  
  def STRING: F[TypeExpr[Unit, String]]
  given sLit: Conversion[String, F[ValueExpr[Unit, String]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def BOOLEAN: StateT[ErrorF, List[Statement], TypeExpr[Unit, Boolean]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.BooleanType(None)))))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, List[Statement], ValueExpr[Unit, Boolean]]] =
      v => StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.BooleanLiteral(None, v)))))

    def INT: StateT[ErrorF, List[Statement], TypeExpr[Unit, Int]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.IntType(None)))))

    given iLit: Conversion[Int, StateT[ErrorF, List[Statement], ValueExpr[Unit, Int]]] =
      v => StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.IntLiteral(None, v)))))

    def STRING: StateT[ErrorF, List[Statement], TypeExpr[Unit, String]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.StringType(None)))))
    
    given sLit: Conversion[String, StateT[ErrorF, List[Statement], ValueExpr[Unit, String]]] =
      v => StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.StringLiteral(None, v)))))