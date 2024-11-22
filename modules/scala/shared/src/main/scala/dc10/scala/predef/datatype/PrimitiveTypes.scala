package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{Error, ErrorF, LibDep, Statement}
import dc10.scala.Statement.TypeExpr.`Type`
import dc10.scala.Statement.ValueExpr.`Value`
import dc10.scala.Symbol.Term

trait PrimitiveTypes[F[_]]:
  type __
  def __ : F[`Type`[__]]

  def BOOLEAN: F[`Type`[Boolean]]
  given bLit: Conversion[Boolean, F[`Value`[Boolean]]]
  
  def INT: F[`Type`[Int]]
  given iLit: Conversion[Int, F[`Value`[Int]]]
  extension (fa: F[`Value`[Int]])
    def +:(fb: F[`Value`[Int]]): F[`Value`[Int]]
    
  def NOTHING: F[`Type`[Nothing]]

  def STRING: F[`Type`[String]]
  given sLit: Conversion[String, F[`Value`[String]]]

  def UNIT: F[`Type`[Unit]]
  given uLit: Conversion[Unit, F[`Value`[Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    type __ = Term.TypeLevel.__

    def __ : StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[__]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("_", None)))

    def BOOLEAN: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[Boolean]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("Boolean", None)))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Boolean]]] =
      v =>
        BOOLEAN.flatMap(t => StateT.pure(Value(Term.ValueLevel.Var.BooleanLiteral(t.tpe, v))))


    def INT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[Int]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("Int", None)))

    given iLit: Conversion[Int, StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Int]]] =
      v => INT.flatMap(t => StateT.pure(Value(Term.ValueLevel.Var.IntLiteral(t.tpe, v))))

    extension (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Int]])
      def +:(fb: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Int]]): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Int]] =
        for
          a <- fa
          b <- fb
          r <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Value[Int]]((a.value, b.value) match
            case (Term.ValueLevel.App.App1(fun, arg, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.App2(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.AppPure(fun, arg, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.AppVargs(fun, tpe, vargs*), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dot0(fun, arg1, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.App.ForComp(gens, ret, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Lam.Lam1(a, b, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.BooleanLiteral(tpe, b), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), Term.ValueLevel.Var.IntLiteral(tpe2, i2)) => Right(Value(Term.ValueLevel.Var.IntLiteral(tpe1, i1 + i2)))
            case (Term.ValueLevel.Var.IntLiteral(tpe1, i1), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.StringLiteral(tpe, s), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.UnitLiteral(tpe, u), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.UserDefinedObject(_, _, _, _), _) => Left(List(Error("Not a value of Int")))
            case (Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl), _) => Left(List(Error("Not a value of Int")))
          )
        yield r
        
    def NOTHING: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[Nothing]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("Nothing", None)))

    def STRING: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[String]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("String", None)))
    
    given sLit: Conversion[String, StateT[ErrorF, (Set[LibDep], List[Statement]), Value[String]]] =
      v => STRING.flatMap(t => StateT.pure(Value(Term.ValueLevel.Var.StringLiteral(t.tpe, v))))

    def UNIT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[Unit]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`("Unit", None)))
    
    given uLit: Conversion[Unit, StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Unit]]] =
      v => UNIT.flatMap(t => StateT.pure(Value(Term.ValueLevel.Var.UnitLiteral(t.tpe, v))))