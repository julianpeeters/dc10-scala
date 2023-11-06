package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{ExtensionDef, TypeExpr, ValueExpr}
import dc10.scala.Symbol.{Extension, Term}
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import org.tpolecat.sourcepos.SourcePos

trait Functions[F[_]]:

  extension [A, B] (domain: F[TypeExpr[A, Unit]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[TypeExpr[B, Unit]]): F[TypeExpr[A => B, Unit]]

  extension [Z, A, B] (domain: F[(TypeExpr[A, Z], TypeExpr[A, Z])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[(A, A) => B, Z]]

  extension [Z, A, B] (fa: F[ValueExpr[A, Z]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: ValueExpr[A, Z] => F[ValueExpr[B, Z]]): F[ValueExpr[A => B, Z]]

  extension [A, B] (fa: F[(ValueExpr[A, Unit], ValueExpr[A, Unit])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => F[ValueExpr[B, Unit]]): F[ValueExpr[(A, A) => B, Unit]]

  def EXT[G[_], B](func: F[G[B]])(using sp: SourcePos): F[G[B]]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B] (domain: StateT[ErrorF, List[Statement], TypeExpr[A, Unit]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Unit]]
      ): StateT[ErrorF, List[Statement], TypeExpr[A => B, Unit]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Unit]](
            Term.TypeLevel.App.App2(
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

    extension [Z, A, B] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Z]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[A, Z] => StateT[ErrorF, List[Statement], ValueExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B, Z]] =
        for
          a <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Z]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Z]](Term.TypeLevel.App.App2(None, Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep), a.value.tpe, b.value.tpe, a.value.tpe.dep))
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[A => B, Z]](Term.ValueLevel.Lam.Lam1(None, a.value, b.value, t))
        yield ValueExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], (ValueExpr[A, Unit], ValueExpr[A, Unit])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => StateT[ErrorF, List[Statement], ValueExpr[B, Unit]]
      ): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => B, Unit]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B, Unit]](Term.TypeLevel.App.App3(None, Term.TypeLevel.Lam.Function2Type(None, ()), a._1.value.tpe, a._2.value.tpe, b.value.tpe, ()))
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[(A, A) => B, Unit]](Term.ValueLevel.Lam.Lam2(None, a._1.value, a._2.value, b.value, t))
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