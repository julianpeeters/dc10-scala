package dc10.scalaq

import cats.data.StateT
import cats.implicits.given
import dc10.scala.dsl.{==>, refV}
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos

type VectorN[_]

trait Vectors[F[_]]:
  def VECTOR[A]: F[TypeExpr[VectorN[A], Unit]]
  def Vector[A]: F[ValueExpr[VectorN[A], Unit]]
  extension [A] (tfunction: F[TypeExpr[VectorN[A], Unit]])
    @scala.annotation.targetName("app1TQ")
    def apply[Z](len: Int, targs: F[TypeExpr[A, Z]]): F[TypeExpr[VectorN[A], (Int, Z)]]
  extension [A] (ctor: F[ValueExpr[VectorN[A], Unit]])
    @scala.annotation.targetName("appVQ1")
    def of[Z](args: F[ValueExpr[A, Z]]*)(using sp: SourcePos): F[ValueExpr[VectorN[A], (Int, Z)]]
  extension [Z, A] (v1: F[ValueExpr[VectorN[A], (Int, Z)]])
    def ++(v2: F[ValueExpr[VectorN[A], (Int, Z)]])(using sp: SourcePos): F[ValueExpr[VectorN[A], (Int, Z)]]

object Vectors:
 
  trait Mixins extends Vectors[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def VECTOR[A]: StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "List", None, ())))
      
    def Vector[A]: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], Unit]] =
      for
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[VectorN[A], Unit]](TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "List", None, ())))
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "List", t.tpe, None)))
      yield v
    
    extension [A] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], Unit]])
      @scala.annotation.targetName("app1TQ")
      def apply[Z](len: Int, targs: StateT[ErrorF, List[Statement], TypeExpr[A, Z]]): StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], (Int, Z)]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Term.TypeLevel.App.App1(None, f.tpe.manageDep(_ => len), a.tpe, (len, a.tpe.dep)))
        
    extension [A] (ctor: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], Unit]])
      @scala.annotation.targetName("appVQ1")
      def of[Z](args: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]*)(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]] =
        for
          l <- ctor
          a <- args.toList.sequence
          h <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Z]](a.headOption.toRight(List(Error(s""))))
          t <- VECTOR.apply(a.length, StateT.pure(TypeExpr(h.value.tpe)))
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel[VectorN[A], (Int, Z)]](
            if (a.forall(e => e.value.tpe.dep == h.value.tpe.dep))
            then Right(Term.ValueLevel.App.AppVargs[VectorN, A, Int, Z](
              None,
              l.value.manageDep(_ => a.length),
              l.value.tpe.manageDep(_ => (a.length, h.value.tpe.dep)),
              a.map(arg => arg.value)*
            ))
            else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Vector error")))
          )
        yield ValueExpr(v)

    extension [Z, A] (vector1: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]])
      def ++(vector2: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]] =
        for
          o <- vector1
          v <- vector2
          f <- o.value match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) =>
              for
                i <- StateT.liftF[ErrorF, List[Statement], Seq[Term.ValueLevel[A, Z]]](o.value.findVargs.toRight(List(Error(""))))
                w <- StateT.liftF[ErrorF, List[Statement], Seq[Term.ValueLevel[A, Z]]](v.value.findVargs.toRight(List(Error(""))))
                n = i.appendedAll(w)
                g <- vector2 ==> ((s: ValueExpr[VectorN[A], (Int, Z)]) => Vector.of(n.map(e => refV(ValueExpr(e)))*))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](
                  ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "++", g.value.tpe, Some(g.value)))
                )
              yield v
            case Term.ValueLevel.Lam.Lam1(_, _, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Lam.Lam2(_, _, _, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.BooleanLiteral(_, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.IntLiteral(_, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.StringLiteral(_, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.ListCtor(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.OptionCtor(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.SomeCtor(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
        yield ValueExpr(Term.ValueLevel.App.Dotless(None, f.value, o.value, v.value, o.value.tpe.manageDep(d => (d._1 + v.value.tpe.dep._1, d._2))))