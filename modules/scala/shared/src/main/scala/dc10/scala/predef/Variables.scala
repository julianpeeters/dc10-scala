package dc10.scala.predef

import cats.data.StateT
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.ctx.ext
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]], impl: ValueExpr[A] => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  def DEF[A, B, T](nme: String, arg1: F[ValueExpr[A]], arg2: F[ValueExpr[B]], tpe: F[TypeExpr[T]], impl: (ValueExpr[A], ValueExpr[B]) => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[(A, B) => T]]
  def F[G[_], T]: F[TypeExpr[T]] => F[TypeExpr[G[T]]]
  def TYPE[T](nme: String): F[TypeExpr[T]]
  def TYPE[T](nme: String, impl: F[TypeExpr[T]]): F[TypeExpr[T]]
  def VAL[T, A](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def VAL[T](nme: String, tpe: F[TypeExpr[T]], impl: F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  given refT[T]: Conversion[TypeExpr[T], F[TypeExpr[T]]]
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
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A]](TypeExpr[A]((a.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, None))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

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
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A]](TypeExpr[A]((a.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def DEF[A, B, T](
      nme: String,
      arg1: StateT[ErrorF, List[Statement], ValueExpr[A]],
      arg2: StateT[ErrorF, List[Statement], ValueExpr[B]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]], 
      impl: (ValueExpr[A], ValueExpr[B]) => StateT[ErrorF, List[Statement], ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(A, B) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a, b)
        t <- StateT.pure[ErrorF, List[Statement], (TypeExpr[A], TypeExpr[B])]((TypeExpr[A](a.value.tpe), TypeExpr[B](b.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], (ValueExpr[A], ValueExpr[B])]((a, b)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def F[G[_], T]: StateT[ErrorF, List[Statement], TypeExpr[T]] => StateT[ErrorF, List[Statement], TypeExpr[G[T]]] =
      s => 
        for
          a <- s
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[G[T]]](Term.TypeLevel.Var.UserDefinedType("F", None))
          d <- StateT.pure(TypeExpr(Term.TypeLevel.App.App1(t, a.tpe)))
        yield d

    def TYPE[T](nme: String): StateT[ErrorF, List[Statement], TypeExpr[T]] =
      for
        t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T]](Term.TypeLevel.Var.UserDefinedType(nme, None))
        d <- StateT.pure(TypeDef.Alias[T](0, t))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def TYPE[T](nme: String, impl: StateT[ErrorF, List[Statement], TypeExpr[T]]): StateT[ErrorF, List[Statement], TypeExpr[T]] =
      for
        i <- StateT.liftF(impl.runEmptyA)
        t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T]](Term.TypeLevel.Var.UserDefinedType(nme, Some(i.tpe)))
        d <- StateT.pure(TypeDef.Alias[T](0, t))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def VAL[T, A](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]], 
      impl: StateT[ErrorF, List[Statement], ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[T]] =
      for
        t <- tpe
        i <- impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(i.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr[T](v)

    given refT[T]: Conversion[TypeExpr[T], StateT[ErrorF, List[Statement], TypeExpr[T]]] =
      t => StateT.pure(t)

    given refV[T]: Conversion[ValueExpr[T], StateT[ErrorF, List[Statement], ValueExpr[T]]] =
      v => StateT.pure(v)