package dc10.scala.predef

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{LibraryDependency, TypeDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.TypeLevel.Var.__
import dc10.scala.ctx.{dep, ext}
import org.tpolecat.sourcepos.SourcePos

trait Variables[F[_]]:
  extension [T] (lhs: F[TypeExpr[T]])
    def :=(rhs: F[TypeExpr[T]]): F[TypeExpr[T]]
  // 0-arg
  def DEF[T](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def DEF[A, T](nme: String, tparam: F[TypeExpr[A]], tpe: TypeExpr[A] => F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def DEF[A, B, T](nme: String, tparam1: F[TypeExpr[A]], tparam2: F[TypeExpr[B]], tpe: (TypeExpr[A], TypeExpr[B]) => F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  // 1-arg
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  def DEF[A, T](nme: String, arg: F[ValueExpr[A]], tpe: F[TypeExpr[T]], impl: ValueExpr[A] => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[A => T]]
  // 2-arg
  def DEF[A, B, T](nme: String, arg1: F[ValueExpr[A]], arg2: F[ValueExpr[B]], tpe: F[TypeExpr[T]], impl: (ValueExpr[A], ValueExpr[B]) => F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[(A, B) => T]]
  def TYPE[T](nme: String): F[TypeExpr[T]]
  def TYPE[G[_], A](nme: String, targ: F[TypeExpr[A]]): F[TypeExpr[G[A]]]
  def TYPE[G[_[_], _], H[_], A](nme: String, targF: F[TypeExpr[H[__]]], targA: F[TypeExpr[A]]): F[TypeExpr[G[H, A]]]
  extension [G[_]] (hkt: F[TypeExpr[G[__]]])
    def apply[A](targ: F[TypeExpr[A]]): F[TypeExpr[G[A]]]    
  def VAL[T, A](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def VAL[T](nme: String, tpe: F[TypeExpr[T]], impl: F[ValueExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  given refT[T]: Conversion[TypeExpr[T], F[TypeExpr[T]]]
  given refV[T]: Conversion[ValueExpr[T], F[ValueExpr[T]]]

object Variables:

  trait Mixins extends Variables[
    [A] =>> StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]
  ] with Functions.Mixins:

    extension [T] (lhs: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]])
      def :=(
        rhs: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
      ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.tpe match
            case Term.TypeLevel.App.App1(tfun, targ) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.App.App1T(tfun, farg, aarg) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.App.App2(tfun, ta, tb) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Lam.Function1Type() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Lam.Function2Type() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.__() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.BooleanType() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.IntType() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.NothingType() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.StringType() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.UnitType() => Left(List(Error("Not an assignable type")))
            case Term.TypeLevel.Var.UserDefinedType(nme, tparams, impl) => Right(Term.TypeLevel.Var.UserDefinedType[T](nme, tparams, Some(r.tpe)))
          )
          d <- StateT.pure(TypeDef.Alias[T](0, t))
          _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        yield TypeExpr[T](t)

    def DEF[T](nme: String, tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]])(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, Nil, None))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, None, t.tpe, None))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)

    def DEF[A, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]],
      tpe: TypeExpr[A] => StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, List(a.tpe), None))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, None, t.tpe, None))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)
      
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]],
      tparam2: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[B]],
      tpe: (TypeExpr[A], TypeExpr[B]) => StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, List(a.tpe, b.tpe), None))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, None, t.tpe, None))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A]], 
      tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]](TypeExpr[A]((a.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, Nil, None))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, Some(a.value), r.tpe, None))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A]],
      tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]], 
      impl: ValueExpr[A] => StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a)
        t <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]](TypeExpr[A]((a.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, Nil, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, Some(a.value), r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)

    def DEF[A, B, T](
      nme: String,
      arg1: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[A]],
      arg2: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[B]],
      tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]], 
      impl: (ValueExpr[A], ValueExpr[B]) => StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[(A, B) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a, b)
        t <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), (TypeExpr[A], TypeExpr[B])]((TypeExpr[A](a.value.tpe), TypeExpr[B](b.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), (ValueExpr[A], ValueExpr[B])]((a, b)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Nil, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Def(0, v, Some(a.value), r.tpe, Some(i.value)))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr(v)

    def TYPE[T](nme: String): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]] =
      for
        t <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), Term.TypeLevel.Var.UserDefinedType[T]](Term.TypeLevel.Var.UserDefinedType(nme, Nil, None))
        d <- StateT.pure(TypeDef.Alias[T](0, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def TYPE[G[_], A](
      nme: String,
      targ: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]]
    ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[A]]] =
      for
        a <- targ
        t <- StateT.pure(Term.TypeLevel.Var.UserDefinedType[G[A]](nme, List(a.tpe), None))
        d <- StateT.pure(TypeDef.Alias[G[A]](0, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[H[__]]],
      targA: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]]
    ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[H, A]]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.UserDefinedType[G[H, A]](nme, List(f.tpe, a.tpe), None))
        d <- StateT.pure(TypeDef.Alias[G[H, A]](0, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield TypeExpr(t)

    extension [G[_]] (hkt: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[__]]])
      def apply[A](targ: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[A]]): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[A]]] =
        for
          f <- hkt
          a <- targ
          r <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[A]]](f.tpe match
            case Term.TypeLevel.App.App1(tfun, targ) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.App.App1T(tfun, farg, aarg) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.App.App2(tfun, ta, tb) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Lam.Function1Type() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Lam.Function2Type() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.__() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.BooleanType() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.IntType() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.NothingType() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.StringType() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.UnitType() => Left(List(Error("Not a higher kinded type")))
            case Term.TypeLevel.Var.UserDefinedType(nme, tparams, impl) => Right(TypeExpr(Term.TypeLevel.Var.UserDefinedType[G[A]](nme, List(a.tpe), None)))
          )
        yield r

    def VAL[T, A](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, Nil, None))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]], 
      impl: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        i <- impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Nil, Some(i.value)))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ValueDef](ValueDef.Val(0, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(l)))
      yield ValueExpr[T](v)

    given refT[T]: Conversion[TypeExpr[T], StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]] =
      t => StateT.pure(t)

    given refV[T]: Conversion[ValueExpr[T], StateT[ErrorF, (Set[LibraryDependency], List[Statement]), ValueExpr[T]]] =
      v => StateT.pure(v)