package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.{ErrorF, ScalaError, TooManyExtensionArguments}
import dc10.scala.Statement
import dc10.scala.Statement.{ExtensionDef, TypeExpr, ValueExpr}
import dc10.scala.Symbol.{Extension, Term}
import dc10.scala.Symbol.Term.{Type, Value}
import dc10.scala.ctx.ext

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

  def EXT[G[_], B](func: F[G[B]]): F[G[B]]

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
          v <- StateT.pure[ErrorF, List[Statement], Type[A => B]](
            Cofree((), Eval.now(Term.TypeLevel.App2(
              None,
              Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))),
              a.tpe,
              b.tpe))
            )
          )
        yield TypeExpr(v)

    extension [A, B] (domain: StateT[ErrorF, List[Statement], (TypeExpr[A], TypeExpr[A])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B]]
      ): StateT[ErrorF, List[Statement], TypeExpr[(A, A) => B]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], Type[(A, A) => B]](
            Cofree((), Eval.now(Term.TypeLevel.App3(
              None,
              Cofree((), Eval.now(Term.TypeLevel.Var.Function2Type(None))),
              a._1.tpe,
              a._2.tpe,
              b.tpe))
            )
          )
        yield TypeExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], ValueExpr[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[A] => StateT[ErrorF, List[Statement], ValueExpr[B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
          v <- StateT.pure[ErrorF, List[Statement], Value[A => B]](
            Cofree((), Eval.now(Term.ValueLevel.Lam.Lam1(None, a.value, b.value)))
          )
        yield ValueExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], (ValueExpr[A], ValueExpr[A])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[A], ValueExpr[A]) => StateT[ErrorF, List[Statement], ValueExpr[B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          v <- StateT.pure[ErrorF, List[Statement], Value[(A, A) => B]](
            Cofree((), Eval.now(Term.ValueLevel.Lam.Lam2(None, a._1.value, a._2.value, b.value)))
          )
        yield ValueExpr(v)

    def EXT[G[_], B](
      func: StateT[ErrorF, List[Statement], G[B]]
    ): StateT[ErrorF, List[Statement], G[B]] =
      for
        (ms, f) <- StateT.liftF(func.runEmpty)
        e <- StateT.liftF(ms match
          case arg1 :: methods => Right(Extension(arg1, methods))
          case Nil => Either.left[List[ScalaError], Extension](List(TooManyExtensionArguments()))
        )
        d <- StateT.pure(ExtensionDef(e, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield f