package dc10.scala.predef

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{Error, ErrorF, LibDep, Statement}
import dc10.scala.Statement.{TypeDef, ValueDef}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_]]`, `Type[_[_], _]`}
import dc10.scala.Statement.ValueExpr.{`Value`}
import dc10.scala.Symbol.Term
import dc10.scala.ctx.{dep, ext}
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  extension [T] (lhs: F[`Type`[T]])
    @scala.annotation.targetName("*")
    def :=(rhs: F[`Type`[T]])(using sp: SourcePos): F[`Type`[T]]
    @scala.annotation.targetName("*->*")
    def :=[G[_], A](rhs: F[`Type[_]`[G]])(using sp: SourcePos): F[`Type[_]`[G]]
  @scala.annotation.targetName("0")
  def DEF[T](nme: String, tpe: F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  @scala.annotation.targetName("0*")
  def DEF[A, T](nme: String, tparam: F[`Type`[A]], tpe: `Type`[A] => F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  @scala.annotation.targetName("0*->*")
  def DEF[G[_], A, T](nme: String, tparam: F[`Type[_]`[G]], tpe: `Type[_]`[G] => F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  @scala.annotation.targetName("0(*->*)->*->*")
  def DEF[A, B, T](nme: String, tparam1: F[`Type`[A]], tparam2: F[`Type`[B]], tpe: (`Type`[A], `Type`[B]) => F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  // 1-arg
  def DEF[A, T](nme: String, arg: F[`Value`[A]], tpe: F[`Type`[T]])(using sp: SourcePos): F[`Value`[A => T]]
  def DEF[A, T](nme: String, arg: F[`Value`[A]], tpe: F[`Type`[T]], impl: Value[A] => F[`Value`[T]])(using sp: SourcePos): F[`Value`[A => T]]
  // 2-arg
  def DEF[A, B, T](nme: String, arg1: F[`Value`[A]], arg2: F[`Value`[B]], tpe: F[`Type`[T]], impl: (Value[A], Value[B]) => F[`Value`[T]])(using sp: SourcePos): F[`Value`[(A, B) => T]]
  @scala.annotation.targetName("*")
  def TYPE[T](nme: String)(using sp: SourcePos): F[`Type`[T]]
  @scala.annotation.targetName("*->*")
  def TYPE[G[_], A](nme: String, tparam: F[`Type`[A]])(using sp: SourcePos): F[`Type[_]`[G]]
  @scala.annotation.targetName("(*->*)->*")
  def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type[_]`[H]])(using sp: SourcePos): F[`Type[_[_]]`[G]]
  def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type[_]`[H]], targA: F[`Type`[A]])(using sp: SourcePos): F[`Type[_[_], _]`[G]]
  def VAL[T, A](nme: String, tpe: F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  def VAL[T](nme: String, tpe: F[`Type`[T]], impl: F[`Value`[T]])(using sp: SourcePos): F[`Value`[T]]
  given `refT`[T]: Conversion[`Type`[T], F[`Type`[T]]]
  given `refT[_]`[T[_]]: Conversion[`Type[_]`[T], F[`Type[_]`[T]]]
  given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type[_[_], _]`[T], F[`Type[_[_], _]`[T]]]
  given refV[T]: Conversion[`Value`[T], F[`Value`[T]]]

object Variables:

  trait Mixins extends Variables[
    [A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A]
  ] with Functions.Mixins:

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]])
      @scala.annotation.targetName("*")
      def :=(
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
      )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.tpe match
            case Term.TypeLevel.App.`App[_]`(tfun, targ) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.Var.`UserDefinedType`(nme, impl) => Right(Term.TypeLevel.Var.`UserDefinedType`(nme, Some(r.tpe)))
          )
          d <- StateT.pure(TypeDef.`Alias`[T](0, sp, t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield Type[T](t)

      @scala.annotation.targetName("*->*")
      def :=[G[_], A](
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]]
      )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.tpe match
            case Term.TypeLevel.App.`App[_]`(tfun, aarg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.Var.UserDefinedType(nme, impl) => Right(Term.TypeLevel.Var.`UserDefinedType[_]`(nme, Some(r.tpe)))
          )
          d <- StateT.pure(TypeDef.`Alias[_]=>>`[G](0, sp, t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield `Type[_]`(t)

    @scala.annotation.targetName("0")
    def DEF[T](
      nme: String, 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, sp, None, None, t.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("0*")
    def DEF[A, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      tpe: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def[_]`(0, sp, a.tpe, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("0*->*")
    def DEF[G[_], A, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
      tpe: `Type[_]`[G] => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        f <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def[_[_]]`(0, sp, f.tpe, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)
      
    @scala.annotation.targetName("0(*->*)->*->*")
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      tparam2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[B]],
      tpe: (`Type`[A], `Type`[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, sp, None, None, t.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]], 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]](`Type`[A]((a.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, sp, Some(a.value), None, r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]],
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: Value[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]](`Type`[A]((a.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, sp, Some(a.value), Some(i.value), r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, B, T](
      nme: String,
      arg1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]],
      arg2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[B]],
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: (Value[A], Value[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[(A, B) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a, b)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), (`Type`[A], `Type`[B])]((`Type`[A](a.value.tpe), `Type`[B](b.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), (`Value`[A], `Value`[B])]((a, b)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, sp, Some(a.value), Some(i.value), r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("*")
    def TYPE[T](nme: String)(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
      for
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.Var.`UserDefinedType`[T]](Term.TypeLevel.Var.`UserDefinedType`(nme, None))
        d <- StateT.pure(TypeDef.`Alias`[T](0, sp, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Type(t)

    @scala.annotation.targetName("*->*")
    def TYPE[G[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_]`(0, sp, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_]`(t)

    @scala.annotation.targetName("(*->*)->*")
    def TYPE[G[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_[_]]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_[_]]`(0, sp, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_[_]]`(t)

    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      targA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[G]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_[_], _]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_[_], _]`[G, H, A](0, sp, f.tpe, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_[_], _]`(t)

    def VAL[T, A](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`val`(0, sp, v, t.tpe))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(v)

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        i <- impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(i.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`val`(0, sp, v, t.tpe))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value[T](v)

    given `refT`[T]: Conversion[`Type`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[T[_]]: Conversion[`Type[_]`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type[_[_], _]`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[T]]] =
      t => StateT.pure(t)

    given refV[T]: Conversion[`Value`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]] =
      v => StateT.pure(v)