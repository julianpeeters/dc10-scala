package dc10.scala.predef

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.ctx.ext
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  def DEF[A, T](nme: String, arg: F[ValueExpr[Unit, A]], tpe: F[TypeExpr[Unit, T]])(using sp: SourcePos): F[ValueExpr[Unit, A => T]]
  def DEF[A, T](nme: String, arg: F[ValueExpr[Unit, A]], tpe: F[TypeExpr[Unit, T]], impl: ValueExpr[Unit, A] => F[ValueExpr[Unit, T]])(using sp: SourcePos): F[ValueExpr[Unit, A => T]]
  def DEF[A, T](nme: String, arg1: F[ValueExpr[Unit, A]], arg2: F[ValueExpr[Unit, A]], tpe: F[TypeExpr[Unit, T]], impl: (ValueExpr[Unit, A], ValueExpr[Unit, A]) => F[ValueExpr[Unit, T]])(using sp: SourcePos): F[ValueExpr[Unit, (A, A) => T]]
  def VAL[Z, T](nme: String, tpe: F[TypeExpr[Z, T]])(using sp: SourcePos): F[ValueExpr[Z, T]]
  def VAL[Z, T](nme: String, tpe: F[TypeExpr[Z, T]], impl: F[ValueExpr[Z, T]])(using sp: SourcePos): F[ValueExpr[Z, T]]
  given refV[Z, T]: Conversion[ValueExpr[Z, T], F[ValueExpr[Z, T]]]

object Variables:

  trait Mixins extends Variables[
    [A] =>> StateT[ErrorF, List[Statement], A]
  ] with Functions.Mixins:

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]], 
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Unit, T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[Unit, A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[Unit, A]](TypeExpr[Unit, A]((a.value.tail.value.tpe.map(_=> ())))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[Unit, A => T, Nothing](None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, None))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Unit, T]], 
      impl: ValueExpr[Unit, A] => StateT[ErrorF, List[Statement], ValueExpr[Unit, T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Unit, A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[Unit, A]](TypeExpr[Unit, A]((a.value.tail.value.tpe.map(_=> ())))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[Unit, A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def DEF[A, T](
      nme: String,
      arg1: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]],
      arg2: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Unit, T]], 
      impl: (ValueExpr[Unit, A], ValueExpr[Unit, A]) => StateT[ErrorF, List[Statement], ValueExpr[Unit, T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Unit, (A, A) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a, a)
        t <- StateT.pure[ErrorF, List[Statement], (TypeExpr[Unit, A], TypeExpr[Unit, A])]((TypeExpr[Unit, A](???), TypeExpr[Unit, A]((???)))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], (ValueExpr[Unit, A], ValueExpr[Unit, A])]((a, a)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def VAL[Z, T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Z, T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[Z, T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree(t.tpe.head, Eval.now(v)))

    def VAL[Z, T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Z, T]], 
      impl: StateT[ErrorF, List[Statement], ValueExpr[Z, T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Z, T]] =
      for
        t <- tpe
        i <- impl
        v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel.Var.UserDefinedValue[Z, T, Nothing]](
          if (t.tpe.head == i.value.head)
            then Right(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(i.value)))
            else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error"))))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr[Z, T](Cofree(t.tpe.head, Eval.now(v)))

    given refV[Z, T]: Conversion[ValueExpr[Z, T], StateT[ErrorF, List[Statement], ValueExpr[Z, T]]] =
      v => StateT.pure(v)