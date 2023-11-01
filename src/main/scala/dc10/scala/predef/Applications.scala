package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.given
import dc10.scala.{Error, Statement}
import dc10.scala.ErrorF
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.TypeLevel.__
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import org.tpolecat.sourcepos.SourcePos

trait Applications[F[_]]:

  extension [T[_]] (function: F[TypeExpr[Unit, T[__]]])
    @scala.annotation.targetName("app1T")
    def apply[A](args: F[TypeExpr[Unit, A]]): F[TypeExpr[Unit, T[A]]]

  extension [T[_,_]] (tfunction: F[TypeExpr[Unit, T[__, __]]])
    @scala.annotation.targetName("app2T")
    def apply[A, B](fta: F[TypeExpr[Unit, A]])(ftb: F[TypeExpr[Unit, B]]): F[TypeExpr[Unit, T[A, B]]]

  extension [A, B] (function: F[ValueExpr[Unit, A => B]])
    @scala.annotation.targetName("app1V")
    def apply(args: F[ValueExpr[Unit, A]])(using sp: SourcePos): F[ValueExpr[Unit, B]]

  extension [A, B] (arg1: F[ValueExpr[Unit, A]] | ValueExpr[Unit, A])
    @scala.annotation.targetName("dot1V_fa|a")
    def DOT(func: F[ValueExpr[Unit, A => B]])(arg2: F[ValueExpr[Unit, B]]): F[ValueExpr[Unit, B]]

  extension [A, B] (arg1: F[ValueExpr[Unit, A]])
    @scala.annotation.targetName("dot1V_fa")
    def DOT(func: F[ValueExpr[Unit, A => B]])(arg2: F[ValueExpr[Unit, B]]): F[ValueExpr[Unit, B]]

object Applications:

  trait Mixins extends Applications[[A] =>> StateT[ErrorF, List[Statement], A]]:

    extension [T[_]] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Unit, T[__]]])
      @scala.annotation.targetName("app1T")
      def apply[A](targs: StateT[ErrorF, List[Statement], TypeExpr[Unit, A]]): StateT[ErrorF, List[Statement], TypeExpr[Unit, T[A]]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Cofree((), Eval.now(Term.TypeLevel.App1(None, f.tpe, a.tpe))))

    extension [T[_,_]] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Unit, T[__, __]]])
      @scala.annotation.targetName("app2T")
      def apply[A, B](fta: StateT[ErrorF, List[Statement], TypeExpr[Unit, A]])(ftb: StateT[ErrorF, List[Statement], TypeExpr[Unit, B]]): StateT[ErrorF, List[Statement], TypeExpr[Unit, T[A, B]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield TypeExpr(Cofree((), Eval.now(Term.TypeLevel.App2(None, f.tpe, a.tpe, b.tpe))))

    extension [A, B] (function: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => B]])
      @scala.annotation.targetName("app1V")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Unit, B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF[ErrorF, List[Statement], Term.Type[Unit, B]](f.value.tail.value.tpe.tail.value match
            case Term.TypeLevel.App1(qnt, tfun, targ) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App2(qnt, tfun, ta, tb) => Right(tb.map(_ => ()))
            case Term.TypeLevel.App3(qnt, tfun, ta1, ta2, tb) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.Function1Type(qnt) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
         
          )
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.App1(None, f.value, a.value, t))))

    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]])
      @scala.annotation.targetName("dot1V_fa")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[Unit, B]]): StateT[ErrorF, List[Statement], ValueExpr[Unit, B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[Unit, B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value))))
        yield ValueExpr(v)

    extension [B] (arg1: String)
      @scala.annotation.targetName("dot1V_s")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[Unit, String => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[Unit, B]]): StateT[ErrorF, List[Statement], ValueExpr[Unit, B]] =
        for
          f <- func
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[Unit, B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, Cofree((), Eval.now(Term.ValueLevel.Var.StringLiteral(None, arg1))), a2.value))))
        yield ValueExpr(v)
    
    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]] | ValueExpr[Unit, A])
      @scala.annotation.targetName("dot1V_fa|a")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[Unit, B]]): StateT[ErrorF, List[Statement], ValueExpr[Unit, B]] =
        for
          f <- StateT.liftF(func.runEmptyA)
          a1 <- arg1 match {
            case u: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]] => u
            case v: ValueExpr[Unit, A] => StateT.pure[ErrorF, List[Statement], ValueExpr[Unit, A]](v)
          }
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[Unit, B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value))))
        yield ValueExpr(v)
