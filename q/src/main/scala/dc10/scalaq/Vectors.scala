package dc10.scalaq

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
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
          v = (f.tpe.head, a.tpe.head)
        yield TypeExpr(Cofree((len, a.tpe.head), Eval.now(Term.TypeLevel.App1(None, f.tpe, a.tpe))))

    extension [A] (ctor: StateT[ErrorF, List[Statement], ValueExpr[Unit, VectorN[A] => VectorN[A]]])
      @scala.annotation.targetName("appVQ1")
      def of[Z](args: StateT[ErrorF, List[Statement], ValueExpr[Z, A]]*)(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(Int, Z), VectorN[A]]] =
        for
          l <- ctor
          a <- args.toList.sequence
          h <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Z, A]](a.headOption.toRight(List(Error(s""))))
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel[VectorN[A], Nothing]](
            if (a.forall(e => e.value.head == h.value.head))
              then Right(Term.ValueLevel.App.AppVargs(None, l.value.map(_ => a.length), a.map(arg => arg.value)*))
              else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error")))
          )
        yield ValueExpr(Cofree((a.length, h.value.head), Eval.now(v)))