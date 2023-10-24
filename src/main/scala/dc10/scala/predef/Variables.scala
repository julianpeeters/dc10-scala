package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.ctx.ext
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]], impl: ValueExpr[A] => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  def DEF[A, T](nme: String, arg1: F[ValueExpr[A]], arg2: F[ValueExpr[A]], tpe: F[TypeExpr[T]], impl: (ValueExpr[A], ValueExpr[A]) => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[(A, A) => T]]
  def VAL[T](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def VAL[T](nme: String, tpe: F[TypeExpr[T]], impl: F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  given refV[T]: Conversion[ValueExpr[T], F[ValueExpr[T]]]

object Variables:

  trait Mixins extends Variables[
    [A] =>> StateT[ErrorF, List[Statement], A]
  ] with Functions.Mixins:

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[A]], 
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A]](TypeExpr[A]((a.value.tail.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, None))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[A]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]], 
      impl: ValueExpr[A] => StateT[ErrorF, List[Statement], ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A]](TypeExpr[A]((a.value.tail.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def DEF[A, T](
      nme: String,
      arg1: StateT[ErrorF, List[Statement], ValueExpr[A]],
      arg2: StateT[ErrorF, List[Statement], ValueExpr[A]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]], 
      impl: (ValueExpr[A], ValueExpr[A]) => StateT[ErrorF, List[Statement], ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a, a)
        t <- StateT.pure[ErrorF, List[Statement], (TypeExpr[A], TypeExpr[A])]((TypeExpr[A](a.value.tail.value.tpe), TypeExpr[A]((b.value.tail.value.tpe)))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], (ValueExpr[A], ValueExpr[A])]((a, a)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]], 
      impl: StateT[ErrorF, List[Statement], ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[T]] =
      for
        t <- tpe
        i <- impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(i.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr[T](Cofree((), Eval.now(v)))

    given refV[T]: Conversion[ValueExpr[T], StateT[ErrorF, List[Statement], ValueExpr[T]]] =
      v => StateT.pure(v)