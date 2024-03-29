package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{ExtensionDef, TypeDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{Extension, Term}
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import dc10.scala.Symbol.Term.TypeLevel.App.Infix
import org.tpolecat.sourcepos.SourcePos
import dc10.scala.Statement.TypeDef.Match
import dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue

trait Functions[F[_]]:

  extension [A, B] (domain: F[TypeExpr[A]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[TypeExpr[B]]): F[TypeExpr[A => B]]

  extension [A, B] (domain: F[(TypeExpr[A], TypeExpr[A])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[TypeExpr[B]]): F[TypeExpr[(A, A) => B]]

  extension [A, B] (fa: F[ValueExpr[A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: ValueExpr[A] => F[ValueExpr[B]]): F[ValueExpr[A => B]]

  extension [A, B] (fa: F[(ValueExpr[A], ValueExpr[A])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (ValueExpr[A], ValueExpr[A]) => F[ValueExpr[B]]): F[ValueExpr[(A, A) => B]]

  def EXT[G[_], B](func: F[G[B]])(using sp: SourcePos): F[G[B]]

  @scala.annotation.targetName("forOption")
  def FOR[A](f: F[ValueExpr[A]])(using sp: SourcePos): F[ValueExpr[Option[A]]]

  extension [G[_], A] (nme: String)
    def <--(ff: F[ValueExpr[G[A]]]): F[ValueExpr[A]]
  
  @scala.annotation.targetName("matchT1")
  def MATCHTYPES[T[_], A, B](nme: String, arg: F[TypeExpr[A]], cases: TypeExpr[A] => F[B])(using sp: SourcePos): F[TypeExpr[T[A]]]
  
  def CASE[A, B](f: F[TypeExpr[A => B]])(using sp: SourcePos): F[Unit]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B] (domain: StateT[ErrorF, List[Statement], TypeExpr[A]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B]]
      ): StateT[ErrorF, List[Statement], TypeExpr[A => B]] =
        for
          a <- domain
          b <- codomain
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B]](
            Term.TypeLevel.App.Infix(
              Term.TypeLevel.Lam.Function1Type(),
              a.tpe,
              b.tpe,
            )
          )
  
        yield TypeExpr(t)

    extension [A, B] (domain: StateT[ErrorF, List[Statement], (TypeExpr[A], TypeExpr[A])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B]]
      ): StateT[ErrorF, List[Statement], TypeExpr[(A, A) => B]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B]](
            Term.TypeLevel.App.App3(
              Term.TypeLevel.Lam.Function2Type(),
              a._1.tpe,
              a._2.tpe,
              b.tpe,
            )
          )
    
        yield TypeExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], ValueExpr[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[A] => StateT[ErrorF, List[Statement], ValueExpr[B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B]] =
        for
          a <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B]](
            Term.TypeLevel.App.Infix(Term.TypeLevel.Lam.Function1Type(), a.value.tpe, b.value.tpe)
          )
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[A => B]](Term.ValueLevel.Lam.Lam1(a.value, b.value, t))
        yield ValueExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], (ValueExpr[A], ValueExpr[A])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[A], ValueExpr[A]) => StateT[ErrorF, List[Statement], ValueExpr[B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B]](Term.TypeLevel.App.App3(Term.TypeLevel.Lam.Function2Type(), a._1.value.tpe, a._2.value.tpe, b.value.tpe))
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[(A, A) => B]](Term.ValueLevel.Lam.Lam2(a._1.value, a._2.value, b.value, t))
        yield ValueExpr(v)





    def EXT[G[_], B](
      func: StateT[ErrorF, List[Statement], G[B]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], G[B]] =
      for
        (ms, f) <- StateT.liftF(func.runEmpty)
        e <- StateT.liftF(ms match
          case arg1 :: methods => Right(Extension(arg1, methods))
          case Nil => Either.left[List[Error], Extension](List(Error(s"${sp.file}:${sp.line}\nToo many extension arguments")))
        )
        d <- StateT.pure(ExtensionDef(e, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield f

    @scala.annotation.targetName("forOption")
    def FOR[A](f: StateT[ErrorF, List[Statement], ValueExpr[A]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      for
        (l, a) <- StateT.liftF(f.runEmpty)
        v <- StateT.pure[ErrorF, List[Statement], ValueLevel[Option[A]]](
          ForComp(l, a.value, Term.TypeLevel.App.App1(Term.TypeLevel.Var.OptionType(), a.value.tpe,
            )))
      yield ValueExpr(v)

    extension [G[_], A] (nme: String)
      def <--(ff: StateT[ErrorF, List[Statement], ValueExpr[G[A]]]): StateT[ErrorF, List[Statement], ValueExpr[A]] =
        for
          g <- ff
          t <- StateT.liftF[ErrorF, List[Statement], TypeLevel[A]](g.value.tpe match
            case dc10.scala.Symbol.Term.TypeLevel.App.App1(tfun, targ) => Right(targ)
            case dc10.scala.Symbol.Term.TypeLevel.App.App1T(tfun, farg, aarg) => Right(aarg.asInstanceOf[TypeLevel[A]])
            case dc10.scala.Symbol.Term.TypeLevel.App.App2(tfun, ta, tb) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.Infix(tfun, ta, tb) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function1Type() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function2Type() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.BooleanType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.IntType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.StringType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.UnitType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.ListType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.OptionType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.SomeType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.TupleType() => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType(nme, impl) => ???
          )
          i <- StateT.liftF[ErrorF, List[Statement], ValueLevel[A]](g.value.findImpl.fold(Left(List(Error(""))))(i => i match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(fun, arg, tpe) => ??? 
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1(tpe, arg) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2(nme, tpe, arg1, arg2) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppPure(fun, arg, tpe) => Right(arg)
            case dc10.scala.Symbol.Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp(l, r, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1(a, b, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral(tpe, b) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral(tpe, i) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral(tpe, s) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral(tpe, s) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor(tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor(tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor(tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.TupleCtor(tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => ???
          )
          )
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel.Var.UserDefinedValue[A]](
                Right(Term.ValueLevel.Var.UserDefinedValue(nme, t, Some(i))))
          d <- StateT.pure(ValueDef.Gen(0, v, g.value))
          _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
        yield ValueExpr(v)

    @scala.annotation.targetName("matchT1")
    def MATCHTYPES[T[_], A, B](
      nme: String,
      arg: StateT[ErrorF, List[Statement], TypeExpr[A]],
      cases: TypeExpr[A] => StateT[ErrorF, List[Statement], B]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[T[A]]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        l <- StateT.liftF(cases(a).runEmptyS)
        c <- StateT.liftF(l.map(s => s match
          case TypeExpr(tpe) => tpe match
            case Infix(tfun, ta, tb) => Right(Infix(tfun, ta, tb))
            case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected function but found ${tpe}")))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected cases but found ${a.tpe}")))
        ).sequence)
        f <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T[A]]](Term.TypeLevel.Var.UserDefinedType(nme, None))
        t <- StateT.liftF(a.tpe match
          case u@Term.TypeLevel.Var.UserDefinedType(_, _) => Right(Term.TypeLevel.App.App1(f, a.tpe))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected user-defined type but found ${a.tpe}")))
        )
        d <- StateT.liftF(c.toNel.fold(Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected at least one case"))))(nel => Right(TypeDef.Match(0, t, nel))))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def CASE[A, B](f: StateT[ErrorF, List[Statement], TypeExpr[A => B]])(using sp: SourcePos): StateT[ErrorF, List[Statement], Unit] =
      for
        a <- f
        _ <- f.modify(d => d :+ a)
      yield ()