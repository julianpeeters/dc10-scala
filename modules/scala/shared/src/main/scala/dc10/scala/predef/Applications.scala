package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{Error, Statement}
import dc10.scala.ErrorF
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import org.tpolecat.sourcepos.SourcePos

trait Applications[F[_]]:

  extension [T[_], A] (function: F[TypeExpr[T[A]]])
    @scala.annotation.targetName("app1T")
    def apply(args: F[TypeExpr[A]]): F[TypeExpr[T[A]]]

  extension [T[_,_], A, B] (tfunction: F[TypeExpr[T[A, B]]])
    @scala.annotation.targetName("app2T")
    def apply(fta: F[TypeExpr[A]], ftb: F[TypeExpr[B]]): F[TypeExpr[T[A, B]]]

  extension [T[_[_],_], G[_], A] (tfunction: F[TypeExpr[T[G, A]]])
    @scala.annotation.targetName("appFA")
    def apply(farg: F[TypeExpr[G[A]]], aarg: F[TypeExpr[A]]): F[TypeExpr[T[G, A]]]

  extension [A, B] (function: F[ValueExpr[A => B]])
    @scala.annotation.targetName("app1V")
    def apply(args: F[ValueExpr[A]])(using sp: SourcePos): F[ValueExpr[B]]

  extension [A, B] (arg1: F[ValueExpr[A]] | ValueExpr[A])
    @scala.annotation.targetName("dot1V_fa|a")
    def DOT(func: F[ValueExpr[A => B]])(arg2: F[ValueExpr[B]]): F[ValueExpr[B]]

  extension [A, B] (arg1: F[ValueExpr[A]])
    @scala.annotation.targetName("dot1V_fa")
    def DOT(func: F[ValueExpr[A => B]])(arg2: F[ValueExpr[B]]): F[ValueExpr[B]]

object Applications:

  trait Mixins extends Applications[[A] =>> StateT[ErrorF, List[Statement], A]]:

    extension [T[_], A] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[A]]])
      @scala.annotation.targetName("app1T")
      def apply(targs: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[T[A]]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Term.TypeLevel.App.App1(
          f.tpe,
          a.tpe
        ))

    extension [T[_[_],_], G[_], A] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[G, A]]])
      @scala.annotation.targetName("appFA")
      def apply(farg: StateT[ErrorF, List[Statement], TypeExpr[G[A]]], aarg: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[T[G, A]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
        yield TypeExpr(Term.TypeLevel.App.App1T(t.tpe, f.tpe, a.tpe))

    extension [T[_,_], A, B] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[A, B]]])
      @scala.annotation.targetName("app2T")
      def apply(fta: StateT[ErrorF, List[Statement], TypeExpr[A]], ftb: StateT[ErrorF, List[Statement], TypeExpr[B]]): StateT[ErrorF, List[Statement], TypeExpr[T[A, B]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield TypeExpr(Term.TypeLevel.App.App2(f.tpe, a.tpe, b.tpe))

    extension [A, B] (function: StateT[ErrorF, List[Statement], ValueExpr[A => B]])
      @scala.annotation.targetName("app1V")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[A]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF[ErrorF, List[Statement], Term.TypeLevel[B]](f.value.tpe match
            case Term.TypeLevel.App.App1(tfun, targ) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.App1T(tfun, farg, aarg) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.App2(tfun, ta, tb) => Right(tb.asInstanceOf[Term.TypeLevel[B]])
            case Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Right(tb.asInstanceOf[Term.TypeLevel[B]])
            case Term.TypeLevel.Lam.Function1Type() => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.NothingType() => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.UserDefinedType(nme, impl) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error")))
          )
        yield ValueExpr(Term.ValueLevel.App.App1(f.value, a.value, t))

    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[A]])
      @scala.annotation.targetName("dot1V_fa")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[A => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B]]): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[B]](Term.ValueLevel.App.Dot1(f.value, a1.value, a2.value, a2.value.tpe))
        yield ValueExpr(v)
    
    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[A]] | ValueExpr[A])
      @scala.annotation.targetName("dot1V_fa|a")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[A => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B]]): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- StateT.liftF(func.runEmptyA)
          a1 <- arg1 match {
            case u: StateT[ErrorF, List[Statement], ValueExpr[A]] => u
            case v: ValueExpr[A] => StateT.pure[ErrorF, List[Statement], ValueExpr[A]](v)
          }
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[B]](Term.ValueLevel.App.Dot1(f.value, a1.value, a2.value, a2.value.tpe))
        yield ValueExpr(v)
