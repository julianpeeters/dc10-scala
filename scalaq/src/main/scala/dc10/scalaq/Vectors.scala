package dc10.scalaq

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.given
import dc10.scala.dsl.{==>, refV}
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.Value.findVargs
import org.tpolecat.sourcepos.SourcePos

type __
type VectorN[__]

trait Vectors[F[_]]:
  def VECTOR: F[TypeExpr[Unit, VectorN[__]]]
  def Vector[A]: F[ValueExpr[Unit, VectorN[A] => VectorN[A]]]
  extension (tfunction: F[TypeExpr[Unit, VectorN[__]]])
    @scala.annotation.targetName("app1TQ")
    def apply[Z, A](len: Int, targs: F[TypeExpr[Z, A]]): F[TypeExpr[(Int, Z), VectorN[A]]]
  extension [A] (ctor: F[ValueExpr[Unit, VectorN[A] => VectorN[A]]])
    @scala.annotation.targetName("appVQ1")
    def of[Z](args: F[ValueExpr[Z, A]]*)(using sp: SourcePos): F[ValueExpr[(Int, Z), VectorN[A]]]
  extension [Z, A] (v1: F[ValueExpr[(Int, Z), VectorN[A]]])
    def ++(v2: F[ValueExpr[(Int, Z), VectorN[A]]])(using sp: SourcePos): F[ValueExpr[(Int, Z), VectorN[A]]]

object Vectors:

  trait Mixins extends Vectors[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def VECTOR: StateT[ErrorF, List[Statement], TypeExpr[Unit, VectorN[__]]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "List", None)))))
      
    def Vector[A]: StateT[ErrorF, List[Statement], ValueExpr[Unit, VectorN[A] => VectorN[A]]] =
      for
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[Unit, VectorN[A] => VectorN[A]]](TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.UserDefinedType(None, "List", None)))))
        v <- StateT.pure(ValueExpr(Cofree(t.tpe.head, Eval.now(Term.ValueLevel.Var.UserDefinedValue(None, "List", t.tpe, None)))))
      yield v
    
    extension (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Unit, VectorN[__]]])
      @scala.annotation.targetName("app1TQ")
      def apply[Z, A](len: Int, targs: StateT[ErrorF, List[Statement], TypeExpr[Z, A]]): StateT[ErrorF, List[Statement], TypeExpr[(Int, Z), VectorN[A]]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Cofree((len, a.tpe.head), Eval.now(Term.TypeLevel.App1(None, f.tpe, a.tpe))))

    extension [A] (ctor: StateT[ErrorF, List[Statement], ValueExpr[Unit, VectorN[A] => VectorN[A]]])
      @scala.annotation.targetName("appVQ1")
      def of[Z](args: StateT[ErrorF, List[Statement], ValueExpr[Z, A]]*)(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A]]] =
        for
          l <- ctor
          a <- args.toList.sequence
          h <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Z, A]](a.headOption.toRight(List(Error(s""))))
          t <- VECTOR.apply(a.length, StateT.pure(TypeExpr(h.value.tail.value.tpe.asInstanceOf[Term.Type[Int, VectorN[A]]])))
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel[VectorN[A], Nothing]](
            if (a.forall(e => e.value.head == h.value.head))
              then Right(Term.ValueLevel.App.AppVargs(
                None,
                l.value.map(_ => a.length),
                Cofree((a.length, h.value.head), Eval.now(Term.TypeLevel.App1(None, t.tpe, h.value.tail.value.tpe))),
                a.map(arg => arg.value)*))
              else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error")))
          )
        yield ValueExpr(Cofree((a.length, h.value.head), Eval.now(v)))

    extension [Z, A] (vector1: StateT[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A]]])
      def ++(vector2: StateT[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A]]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A]]] =
        for
          o <- vector1
          v <- vector2
          f <- o.value.tail.value match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.Println(qnt, s) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) =>
              for
                i <- StateT.liftF[ErrorF, List[Statement], Seq[Term.Value[Z, A]]](o.value.findVargs.toRight(List(Error(""))))
                w <- StateT.liftF[ErrorF, List[Statement], Seq[Term.Value[Z, A]]](v.value.findVargs.toRight(List(Error(""))))
                g <- vector2 ==> ((s: ValueExpr[(Int, Z), VectorN[A]]) => Vector.of(i.appendedAll(w).map(e => refV(ValueExpr(e)))*))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](
                  ValueExpr(Cofree(o.value.head, Eval.now(Term.ValueLevel.Var.UserDefinedValue(None, "++", g.value.tail.value.tpe.asInstanceOf[Term.Type[(Int, Z), VectorN[A] => VectorN[A]]],Some(g.value)))))
                )
              yield v
            case Term.ValueLevel.Lam.Lam1(_, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Lam.Lam2(_, _, _, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.BooleanLiteral(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.IntLiteral(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.StringLiteral(_, _) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.ListCtor(_) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.OptionCtor(_) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
            case Term.ValueLevel.Var.OptionCtor.SomeCtor(_) => StateT.liftF[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A] => VectorN[A]]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
        yield ValueExpr(Cofree(o.value.head.copy(_1 = o.value.head._1 + v.value.head._1), Eval.now(Term.ValueLevel.App.Dotless(None, f.value, o.value, v.value))))