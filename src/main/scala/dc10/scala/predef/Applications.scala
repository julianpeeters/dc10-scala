package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{Error, Statement}
import dc10.scala.ErrorF
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import dc10.scala.Symbol.Term.TypeLevel.dep
import org.tpolecat.sourcepos.SourcePos

trait Applications[F[_]]:

  extension [T[_], A, Y] (function: F[TypeExpr[T[A], Y]])
    @scala.annotation.targetName("app1T")
    def apply[Z](args: F[TypeExpr[A, Z]]): F[TypeExpr[T[A], (Y, Z)]]

  extension [T[_,_], A, B, X, Y, Z] (tfunction: F[TypeExpr[T[A, B], X]])
    @scala.annotation.targetName("app2T")
    def apply(fta: F[TypeExpr[A, Y]], ftb: F[TypeExpr[B, Z]]): F[TypeExpr[T[A, B], (X, (Y, Z))]]

  extension [A, B, Y, Z] (function: F[ValueExpr[A => B, Y]])
    @scala.annotation.targetName("app1V")
    def apply(args: F[ValueExpr[A, Y]])(using sp: SourcePos): F[ValueExpr[B, Y]]

  extension [A, B] (arg1: F[ValueExpr[A, Unit]] | ValueExpr[A, Unit])
    @scala.annotation.targetName("dot1V_fa|a")
    def DOT(func: F[ValueExpr[A => B, Unit]])(arg2: F[ValueExpr[B, Unit]]): F[ValueExpr[B, Unit]]

  extension [A, B] (arg1: F[ValueExpr[A, Unit]])
    @scala.annotation.targetName("dot1V_fa")
    def DOT(func: F[ValueExpr[A => B, Unit]])(arg2: F[ValueExpr[B, Unit]]): F[ValueExpr[B, Unit]]

object Applications:

  trait Mixins extends Applications[[A] =>> StateT[ErrorF, List[Statement], A]]:

    extension [T[_], A, Y] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[A], Y]])
      @scala.annotation.targetName("app1T")
      def apply[Z](targs: StateT[ErrorF, List[Statement], TypeExpr[A, Z]]): StateT[ErrorF, List[Statement], TypeExpr[T[A], (Y, Z)]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Term.TypeLevel.App.App1(None, f.tpe, a.tpe, (f.tpe.dep, a.tpe.dep)))

    extension [T[_,_], A, B, X, Y, Z] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[A, B], X]])
      @scala.annotation.targetName("app2T")
      def apply(fta: StateT[ErrorF, List[Statement], TypeExpr[A, Y]], ftb: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]): StateT[ErrorF, List[Statement], TypeExpr[T[A, B], (X, (Y, Z))]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield TypeExpr(Term.TypeLevel.App.App2(None, f.tpe, a.tpe, b.tpe, (f.tpe.dep, (a.tpe.dep, b.tpe.dep))))

    extension [A, B, Y, Z] (function: StateT[ErrorF, List[Statement], ValueExpr[A => B, Y]])
      @scala.annotation.targetName("app1V")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[B, Y]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF[ErrorF, List[Statement], Term.TypeLevel[B, Y]](f.value.tpe match
            case Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => Right(tb.asInstanceOf[Term.TypeLevel[B, Y]])
            case Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => Right(tb.asInstanceOf[Term.TypeLevel[B, Y]])
            case Term.TypeLevel.Lam.Function1Type(qnt, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error")))
            case Term.TypeLevel.Var.ListType(_, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.OptionType(_, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
            case Term.TypeLevel.Var.SomeType(_, dep) => Left(List(Error(s"${sp.file}:${sp.line}\nApplication Error"))) 
          )
        yield ValueExpr(Term.ValueLevel.App.App1(None, f.value, a.value, t))

    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]])
      @scala.annotation.targetName("dot1V_fa")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[A => B, Unit]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B, Unit]]): StateT[ErrorF, List[Statement], ValueExpr[B, Unit]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[B, Unit]](Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value, ???))
        yield ValueExpr(v)
    
    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]] | ValueExpr[A, Unit])
      @scala.annotation.targetName("dot1V_fa|a")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[A => B, Unit]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B, Unit]]): StateT[ErrorF, List[Statement], ValueExpr[B, Unit]] =
        for
          f <- StateT.liftF(func.runEmptyA)
          a1 <- arg1 match {
            case u: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]] => u
            case v: ValueExpr[A, Unit] => StateT.pure[ErrorF, List[Statement], ValueExpr[A, Unit]](v)
          }
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[B, Unit]](Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value, a2.value.tpe))
        yield ValueExpr(v)
