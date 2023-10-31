package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{ExtensionDef, TypeExpr, ValueExpr}
import dc10.scala.Symbol.{Extension, Term}
import dc10.scala.Symbol.Term.{Type, Value}
import dc10.scala.ctx.ext
import org.tpolecat.sourcepos.SourcePos

trait Functions[F[_]]:

  extension [A, B] (domain: F[TypeExpr[Unit, A]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[TypeExpr[Unit, B]]): F[TypeExpr[Unit, A => B]]

  extension [Z, A, B] (domain: F[(TypeExpr[Z, A], TypeExpr[Z, A])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[TypeExpr[Z, B]]): F[TypeExpr[Z, (A, A) => B]]

  extension [A, B] (fa: F[ValueExpr[Unit, A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: ValueExpr[Unit, A] => F[ValueExpr[Unit, B]]): F[ValueExpr[Unit, A => B]]

  extension [A, B] (fa: F[(ValueExpr[Unit, A], ValueExpr[Unit, A])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (ValueExpr[Unit, A], ValueExpr[Unit, A]) => F[ValueExpr[Unit, B]]): F[ValueExpr[Unit, (A, A) => B]]

  def EXT[G[_], B](func: F[G[B]])(using sp: SourcePos): F[G[B]]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B] (domain: StateT[ErrorF, List[Statement], TypeExpr[Unit, A]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[Unit, B]]
      ): StateT[ErrorF, List[Statement], TypeExpr[Unit, A => B]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], Type[Unit, A => B]](
            Cofree((), Eval.now(Term.TypeLevel.App2(
              None,
              Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))),
              a.tpe,
              b.tpe))
            )
          )
        yield TypeExpr(v)

    extension [Z, A, B] (domain: StateT[ErrorF, List[Statement], (TypeExpr[Z, A], TypeExpr[Z, A])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[Z, B]]
      ): StateT[ErrorF, List[Statement], TypeExpr[Z, (A, A) => B]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], Type[Z, (A, A) => B]](
            Cofree(???, Eval.now(Term.TypeLevel.App3(
              None,
              Cofree(???, Eval.now(Term.TypeLevel.Var.Function2Type(None))),
              a._1.tpe,
              a._2.tpe,
              b.tpe))
            )
          )
        yield TypeExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[Unit, A] => StateT[ErrorF, List[Statement], ValueExpr[Unit, B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[Unit, A => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
          v <- StateT.pure[ErrorF, List[Statement], Value[Unit, A => B]](
            Cofree((), Eval.now(Term.ValueLevel.Lam.Lam1(None, a.value, b.value)))
          )
        yield ValueExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], (ValueExpr[Unit, A], ValueExpr[Unit, A])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[Unit, A], ValueExpr[Unit, A]) => StateT[ErrorF, List[Statement], ValueExpr[Unit, B]]
      ): StateT[ErrorF, List[Statement], ValueExpr[Unit, (A, A) => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          v <- StateT.pure[ErrorF, List[Statement], Value[Unit, (A, A) => B]](
            Cofree((), Eval.now(Term.ValueLevel.Lam.Lam2(None, a._1.value, a._2.value, b.value)))
          )
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