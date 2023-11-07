package dc10.scala.predef

import cats.data.StateT
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.ctx.ext
import dc10.scala.Symbol.Term.TypeLevel.dep
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  def DEF[A, T](nme: String, arg: F[ValueExpr[A, Unit]], tpe: F[TypeExpr[T, Unit]])(using sp: SourcePos): F[ValueExpr[A => T, Unit]]
  def DEF[A, T](nme: String, arg: F[ValueExpr[A, Unit]], tpe: F[TypeExpr[T, Unit]], impl: ValueExpr[A, Unit] => F[ValueExpr[T, Unit]])(using sp: SourcePos): F[ValueExpr[A => T, Unit]]
  def DEF[A, T](nme: String, arg1: F[ValueExpr[A, Unit]], arg2: F[ValueExpr[A, Unit]], tpe: F[TypeExpr[T, Unit]], impl: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => F[ValueExpr[T, Unit]])(using sp: SourcePos): F[ValueExpr[(A, A) => T, Unit]]
  def TYPE[T](nme: String): F[TypeExpr[T, Unit]]
  def TYPE[T, Z](nme: String, impl: F[TypeExpr[T, Z]]): F[TypeExpr[T, Z]]
  def VAL[Z, T, A](nme: String, tpe: F[TypeExpr[T, Z]])(using sp: SourcePos): F[ValueExpr[T, Z]]
  def VAL[Z, T](nme: String, tpe: F[TypeExpr[T, Z]], impl: F[ValueExpr[T, Z]])(using sp: SourcePos): F[ValueExpr[T, Z]]
  given refT[Z, T]: Conversion[TypeExpr[T, Z], F[TypeExpr[T, Z]]]
  given refV[Z, T]: Conversion[ValueExpr[T, Z], F[ValueExpr[T, Z]]]

object Variables:

  trait Mixins extends Variables[
    [A] =>> StateT[ErrorF, List[Statement], A]
  ] with Functions.Mixins:

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]], 
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Unit]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[A => T, Unit]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A, Unit]](TypeExpr[A, Unit]((a.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T, Unit](None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, None))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Unit]], 
      impl: ValueExpr[A, Unit] => StateT[ErrorF, List[Statement], ValueExpr[T, Unit]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[A => T, Unit]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a)
        t <- StateT.pure[ErrorF, List[Statement], TypeExpr[A, Unit]](TypeExpr[A, Unit]((a.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A, Unit]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def DEF[A, T](
      nme: String,
      arg1: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]],
      arg2: StateT[ErrorF, List[Statement], ValueExpr[A, Unit]],
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Unit]], 
      impl: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => StateT[ErrorF, List[Statement], ValueExpr[T, Unit]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => T, Unit]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        r <- StateT.liftF(tpe.runEmptyA)
        i <- impl(a, a)
        t <- StateT.pure[ErrorF, List[Statement], (TypeExpr[A, Unit], TypeExpr[A, Unit])]((TypeExpr[A, Unit](a.value.tpe), TypeExpr[A, Unit](b.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, List[Statement], (ValueExpr[A, Unit], ValueExpr[A, Unit])]((a, a)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Def(0, v, a.value, r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def TYPE[T](nme: String): StateT[ErrorF, List[Statement], TypeExpr[T, Unit]] =
      for
        t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T, Unit]](Term.TypeLevel.Var.UserDefinedType(None, nme, None, ()))
        d <- StateT.pure(TypeDef.Alias[T, Unit](0, t))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def TYPE[T, Z](nme: String, impl: StateT[ErrorF, List[Statement], TypeExpr[T, Z]]): StateT[ErrorF, List[Statement], TypeExpr[T, Z]] =
      for
        i <- StateT.liftF(impl.runEmptyA)
        t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T, Z]](Term.TypeLevel.Var.UserDefinedType(None, nme, Some(i.tpe), i.tpe.dep))
        d <- StateT.pure(TypeDef.Alias[T, Z](0, t))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def VAL[Z, T, A](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Z]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[T, Z]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T, Z](None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def VAL[Z, T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Z]], 
      impl: StateT[ErrorF, List[Statement], ValueExpr[T, Z]]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[T, Z]] =
      for
        t <- tpe
        i <- impl
        v <- StateT.liftF(
          if (t.tpe.dep == i.value.tpe.dep)
            then Right(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(i.value)))
            else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error: ${t.tpe.dep} =/= ${i.value.tpe.dep}"))))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr[T, Z](v)

    given refT[Z, T]: Conversion[TypeExpr[T, Z], StateT[ErrorF, List[Statement], TypeExpr[T, Z]]] =
      t => StateT.pure(t)

    given refV[Z, T]: Conversion[ValueExpr[T, Z], StateT[ErrorF, List[Statement], ValueExpr[T, Z]]] =
      v => StateT.pure(v)