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

  extension [A, B, Y, Z] (domain: F[TypeExpr[A, Y]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[A => B, Unit]]

  extension [Z, A, B] (domain: F[(TypeExpr[A, Z], TypeExpr[A, Z])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[(A, A) => B, Z]]

  extension [A, B, X, Y] (fa: F[ValueExpr[A, Y]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: ValueExpr[A, Y] => F[ValueExpr[B, X]]): F[ValueExpr[A => B, Unit]]

  extension [A, B, Z] (fa: F[(ValueExpr[A, Z], ValueExpr[A, Z])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (ValueExpr[A, Z], ValueExpr[A, Z]) => F[ValueExpr[B, Z]]): F[ValueExpr[(A, A) => B, Z]]

  def EXT[G[_], B](func: F[G[B]])(using sp: SourcePos): F[G[B]]

  @scala.annotation.targetName("forOption")
  def FOR[A, Y](f: F[ValueExpr[A, Y]])(using sp: SourcePos): F[ValueExpr[Option[A], (Unit, Y)]]

  extension [G[_], A, X, Y] (nme: String)
    def <--(ff: F[ValueExpr[G[A], (X,Y)]]): F[ValueExpr[A, Y]]
  
  @scala.annotation.targetName("matchT1")
  def MATCHTYPES[T[_], A, B, Y](nme: String, arg: F[TypeExpr[A, Y]], cases: TypeExpr[A, Y] => F[B])(using sp: SourcePos): F[TypeExpr[T[A], Y]]
  
  def CASE[A, B, X, Z](f: F[TypeExpr[A => B, Z]])(using sp: SourcePos): F[Unit]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B, Y, Z] (domain: StateT[ErrorF, List[Statement], TypeExpr[A, Y]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], TypeExpr[A => B, Unit]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Unit]](
            Term.TypeLevel.App.Infix(
              None,
              Term.TypeLevel.Lam.Function1Type(None, ()),
              a.tpe,
              b.tpe,
              ()
            )
          )
  
        yield TypeExpr(v)

    extension [Z, A, B] (domain: StateT[ErrorF, List[Statement], (TypeExpr[A, Z], TypeExpr[A, Z])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], TypeExpr[(A, A) => B, Z]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B, Z]](
            Term.TypeLevel.App.App3(
              None,
              Term.TypeLevel.Lam.Function2Type(None, b.tpe.dep),
              a._1.tpe,
              a._2.tpe,
              b.tpe,
              b.tpe.dep
            )
          )
    
        yield TypeExpr(v)

    extension [A, B, X, Y] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[A, Y] => StateT[ErrorF, List[Statement], ValueExpr[B, X]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B, Unit]] =
        for
          a <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Y]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Unit]](
            // Term.TypeLevel.App.App2(None, Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep), a.value.tpe, b.value.tpe, ())
            Term.TypeLevel.App.Infix(None, Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep), a.value.tpe, b.value.tpe, ())
          )
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[A => B, Unit]](Term.ValueLevel.Lam.Lam1(None, a.value, b.value, t))
        yield ValueExpr(v)

    extension [A, B, Z] (fa: StateT[ErrorF, List[Statement], (ValueExpr[A, Z], ValueExpr[A, Z])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[A, Z], ValueExpr[A, Z]) => StateT[ErrorF, List[Statement], ValueExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => B, Z]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B, Z]](Term.TypeLevel.App.App3(None, Term.TypeLevel.Lam.Function2Type(None, a._1.value.tpe.dep), a._1.value.tpe, a._2.value.tpe, b.value.tpe, a._2.value.tpe.dep))
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[(A, A) => B, Z]](Term.ValueLevel.Lam.Lam2(None, a._1.value, a._2.value, b.value, t))
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
    def FOR[A, Y](f: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Option[A], (Unit, Y)]] =
      for
        (l, a) <- StateT.liftF(f.runEmpty)
        v <- StateT.pure[ErrorF, List[Statement], ValueLevel[Option[A], (Unit, Y)]](
          ForComp(None, l, a.value, Term.TypeLevel.App.App1(None, Term.TypeLevel.Var.OptionType(None, ()), a.value.tpe, ((), a.value.tpe.dep))))
      yield ValueExpr(v)

    extension [G[_], A, X, Y] (nme: String)
      def <--(ff: StateT[ErrorF, List[Statement], ValueExpr[G[A], (X, Y)]]): StateT[ErrorF, List[Statement], ValueExpr[A, Y]] =
        for
          g <- ff
          t <- StateT.liftF[ErrorF, List[Statement], TypeLevel[A, Y]](g.value.tpe match
            case dc10.scala.Symbol.Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => Right(targ.asInstanceOf[TypeLevel[A, Y]])
            case dc10.scala.Symbol.Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.App2T(qnt, tfun, ta1, ta2, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function1Type(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function2Type(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.BooleanType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.IntType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.StringType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.UnitType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.ListType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.OptionType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.SomeType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => ???
          )
          i <- StateT.liftF[ErrorF, List[Statement], ValueLevel[A, Y]](g.value.findImpl.fold(Left(List(Error(""))))(i => i match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => ??? 
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2(qnt, tpe, arg1, arg2) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Right(arg.asInstanceOf[ValueLevel[A, Y]])
            case dc10.scala.Symbol.Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp(qnt, l, r, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2(qnt, a1, a2, c, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral(qnt, tpe, s) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => ???
          )
          )
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel.Var.UserDefinedValue[A, Y]](
                Right(Term.ValueLevel.Var.UserDefinedValue(None, nme, t, Some(i))))
          d <- StateT.pure(ValueDef.Gen(0, v, g.value))
          _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
        yield ValueExpr(v)

    @scala.annotation.targetName("matchT1")
    def MATCHTYPES[T[_], A, B, Y](
      nme: String,
      arg: StateT[ErrorF, List[Statement], TypeExpr[A, Y]],
      cases: TypeExpr[A, Y] => StateT[ErrorF, List[Statement], B]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[T[A], Y]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        l <- StateT.liftF(cases(a).runEmptyS)
        c <- StateT.liftF(l.map(s => s match
          case TypeExpr(tpe) => tpe match
            case Infix(qnt, tfun, ta, tb, dep) => Right(Infix(qnt, tfun, ta, tb, dep))
            case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected function but found ${tpe}")))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected cases but found ${a.tpe}")))
        ).sequence)
        f <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T[A], Unit]](Term.TypeLevel.Var.UserDefinedType(None, nme, None, ()))
        t <- StateT.liftF(a.tpe match
          case u@Term.TypeLevel.Var.UserDefinedType(_, _, _, _) => Right(Term.TypeLevel.App.App1(None, f, a.tpe, (a.tpe.dep)))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected user-defined type but found ${a.tpe}")))
        )
        d <- StateT.liftF(c.toNel.fold(Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected at least one case"))))(nel => Right(TypeDef.Match(0, t, nel))))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def CASE[A, B, X, Z](f: StateT[ErrorF, List[Statement], TypeExpr[A => B, Z]])(using sp: SourcePos): StateT[ErrorF, List[Statement], Unit] =
      for
        a <- f
        _ <- f.modify(d => d :+ a)
      yield ()