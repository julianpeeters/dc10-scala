package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.{Statement}
import dc10.scala.ErrorF
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.TypeLevel.__
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import dc10.scala.Symbol.Term.ValueLevel.App.App1
// import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
// import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
// import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
// import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
// import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
// import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor.SomeCtor
// import dc10.scala.Symbol.Term.ValueLevel.Var.Println
// import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue
import dc10.scala.TooManyExtensionArguments
import dc10.scala.Symbol.Term.TypeLevel.App1
import dc10.scala.Symbol.Term.TypeLevel.App2
import dc10.scala.Symbol.Term.TypeLevel.App3
// import dc10.scala.Symbol.Term.TypeLevel.Var.BooleanType
// import dc10.scala.Symbol.Term.TypeLevel.Var.IntType
// import dc10.scala.Symbol.Term.TypeLevel.Var.StringType
import dc10.scala.Symbol.Term.TypeLevel.Var.Function1Type
// import dc10.scala.Symbol.Term.TypeLevel.Var.Function2Type
// import dc10.scala.Symbol.Term.TypeLevel.Var.ListType
// import dc10.scala.Symbol.Term.TypeLevel.Var.OptionType
// import dc10.scala.Symbol.Term.TypeLevel.Var.OptionType.SomeType
import dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType

trait Applications[F[_]]:

  extension [T[_]] (function: F[TypeExpr[T[__]]])
    @scala.annotation.targetName("app1T")
    def apply[A](args: F[TypeExpr[A]]): F[TypeExpr[T[A]]]

  extension [T[_,_]] (tfunction: F[TypeExpr[T[__, __]]])
    @scala.annotation.targetName("app2T")
    def apply[A, B](fta: F[TypeExpr[A]])(ftb: F[TypeExpr[B]]): F[TypeExpr[T[A, B]]]

  extension [A, B] (function: F[ValueExpr[A => B]])
    @scala.annotation.targetName("app1V")
    def apply(args: F[ValueExpr[A]]): F[ValueExpr[B]]

  extension [A, B] (arg1: F[ValueExpr[A]] | ValueExpr[A])
    @scala.annotation.targetName("dot1V_fa|a")
    def DOT(func: F[ValueExpr[A => B]])(arg2: F[ValueExpr[B]]): F[ValueExpr[B]]

  extension [A, B] (arg1: F[ValueExpr[A]])
    @scala.annotation.targetName("dot1V_fa")
    def DOT(func: F[ValueExpr[A => B]])(arg2: F[ValueExpr[B]]): F[ValueExpr[B]]

object Applications:

  trait Mixins extends Applications[[A] =>> StateT[ErrorF, List[Statement], A]]:

    extension [T[_]] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[__]]])
      @scala.annotation.targetName("app1T")
      def apply[A](targs: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[T[A]]] =
        for
          f <- tfunction
          a <- targs
        yield TypeExpr(Cofree((), Eval.now(Term.TypeLevel.App1(None, f.tpe, a.tpe))))

    extension [T[_,_]] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[T[__, __]]])
      @scala.annotation.targetName("app2T")
      def apply[A, B](fta: StateT[ErrorF, List[Statement], TypeExpr[A]])(ftb: StateT[ErrorF, List[Statement], TypeExpr[B]]): StateT[ErrorF, List[Statement], TypeExpr[T[A, B]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield TypeExpr(Cofree((), Eval.now(Term.TypeLevel.App2(None, f.tpe, a.tpe, b.tpe))))

    extension [A, B] (function: StateT[ErrorF, List[Statement], ValueExpr[A => B]])
      @scala.annotation.targetName("app1V")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF[ErrorF, List[Statement], Term.Type[B]](f.value.tail.value.tpe.tail.value match
            case Term.TypeLevel.App1(qnt, tfun, targ) => Left(List(TooManyExtensionArguments())) 
            case Term.TypeLevel.App2(qnt, tfun, ta, tb) => Right(tb)
            case Term.TypeLevel.App3(qnt, tfun, ta1, ta2, tb) => Left(List(TooManyExtensionArguments())) 
            case Term.TypeLevel.Var.Function1Type(qnt) => Left(List(TooManyExtensionArguments())) 
            case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl) => Left(List(TooManyExtensionArguments())) 
         
          )
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.App1(None, f.value, a.value, t))))

    extension [A, B] (arg1: StateT[ErrorF, List[Statement], ValueExpr[A]])
      @scala.annotation.targetName("dot1V_fa")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[A => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B]]): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value))))
        yield ValueExpr(v)

    extension [B] (arg1: String)
      @scala.annotation.targetName("dot1V_s")
      def DOT(func: StateT[ErrorF, List[Statement], ValueExpr[String => B]])(arg2: StateT[ErrorF, List[Statement], ValueExpr[B]]): StateT[ErrorF, List[Statement], ValueExpr[B]] =
        for
          f <- func
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, Cofree((), Eval.now(Term.ValueLevel.Var.StringLiteral(None, arg1))), a2.value))))
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
          v <- StateT.pure[ErrorF, List[Statement], Term.Value[B]](Cofree((), Eval.now(Term.ValueLevel.App.Dot1(None, f.value, a1.value, a2.value))))
        yield ValueExpr(v)
