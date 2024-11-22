package dc10.scala.predef

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{Error, ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.{TypeDef, ValueDef}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_]]`, `Type[_[_], _]`}
import dc10.scala.Statement.ValueExpr.{`Value`, `Value[_[_], _]`}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.App2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot0
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.App.ForComp
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedObject
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue
import dc10.scala.Statement.`case class`
import dc10.scala.Statement.extension
import dc10.scala.Statement.`object`
import dc10.scala.Statement.TraitDef.`trait`
import dc10.scala.Statement.TraitDef.`trait[_]`
import dc10.scala.Statement.TraitDef.`trait[_[_]]`
import dc10.scala.Statement.TraitDef.`trait[_[_], _]`
import dc10.scala.Statement.TypeDef.Alias
import dc10.scala.Statement.TypeDef.`Alias[_]`
import dc10.scala.Statement.TypeDef.`Alias[_]=>>`
import dc10.scala.Statement.TypeDef.`Alias[_[_]]`
import dc10.scala.Statement.TypeDef.`Alias[_[_], _]`
import dc10.scala.Statement.TypeDef.Match
import dc10.scala.Statement.ValueDef.`def`
import dc10.scala.Statement.ValueDef.`def[_]`
import dc10.scala.Statement.ValueDef.`def[_[_]]`
import dc10.scala.Statement.ValueDef.`def[_[_], _]`
import dc10.scala.Statement.ValueDef.Fld
import dc10.scala.Statement.ValueDef.Gen
import dc10.scala.Statement.ValueDef.`val`
import dc10.scala.Statement.TypeExpr.`Type[_, _]`
import dc10.scala.Statement.TypeExpr.`Type[_[_], _, _]`

trait Variables[F[_]]:
  extension [T] (lhs: F[`Type`[T]])
    @scala.annotation.targetName("*")
    def :=(rhs: F[`Type`[T]]): F[`Type`[T]]
    @scala.annotation.targetName("*->*")
    def :=[G[_], A](rhs: F[`Type[_]`[G]]): F[`Type[_]`[G]]
  extension [T] (lhs: F[`Value`[T]])
    def :=(rhs: F[`Value`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("0")
  def DEF[T](nme: String, tpe: F[`Type`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("0*")
  def DEF[A, T](nme: String, tparam: F[`Type`[A]], tpe: `Type`[A] => F[`Type`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("0*->*->*")
  def DEF[A, B, T](nme: String, tparam1: F[`Type`[A]], tparam2: F[`Type`[B]], tpe: (`Type`[A], `Type`[B]) => F[`Type`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("0*->*")
  def DEF[G[_], T](nme: String, tparam: F[`Type[_]`[G]], tpe: `Type[_]`[G] => F[`Type`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("0(*->*)->*->*")
  def DEF[G[_], A, T](nme: String, tparamf: F[`Type[_]`[G]], tparama: F[`Type`[A]], tpe: (`Type[_]`[G], `Type`[A]) => F[`Type`[T]]): F[`Value`[T]]
  // 1-arg
  def DEF[A, T](nme: String, arg: F[`Value`[A]], tpe: F[`Type`[T]]): F[`Value`[A => T]]
  def DEF[A, T](nme: String, arg: F[`Value`[A]], tpe: F[`Type`[T]], impl: Value[A] => F[`Value`[T]]): F[`Value`[A => T]]
  // 2-arg
  def DEF[A, B, T](nme: String, arg1: F[`Value`[A]], arg2: F[`Value`[B]], tpe: F[`Type`[T]], impl: (Value[A], Value[B]) => F[`Value`[T]]): F[`Value`[(A, B) => T]]
  @scala.annotation.targetName("*")
  def TYPE[T](nme: String): F[`Type`[T]]
  @scala.annotation.targetName("*->*")
  def TYPE[G[_], A](nme: String, tparam: F[`Type`[A]]): F[`Type[_]`[G]]
  @scala.annotation.targetName("(*->*)->*")
  def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type[_]`[H]]): F[`Type[_[_]]`[G]]
  def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type[_]`[H]], targA: F[`Type`[A]]): F[`Type[_[_], _]`[G]]
  def VAL[T](nme: String, tpe: F[`Type`[T]]): F[`Value`[T]]
  def VAL[T](nme: String, tpe: F[`Type`[T]], impl: F[`Value`[T]]): F[`Value`[T]]
  given `refT`[T]: Conversion[`Type`[T], F[`Type`[T]]]
  given `refT[_]`[T[_]]: Conversion[`Type[_]`[T], F[`Type[_]`[T]]]
  given `refT[_[_]]`[T[_[_]]]: Conversion[`Type[_[_]]`[T], F[`Type[_[_]]`[T]]]
  given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type[_[_], _]`[T], F[`Type[_[_], _]`[T]]]
  given refV[T]: Conversion[`Value`[T], F[`Value`[T]]]

object Variables:

  trait Mixins extends Variables[
    StateT[ErrorF, (Set[LibDep], List[Statement]), _]
  ] with Functions.Mixins:

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]])
      @scala.annotation.targetName("*")
      def :=(
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.tpe match
            case Term.TypeLevel.App.`App[_]`(tfun, targ) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_[_]]`(tfun, farg) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_[_], _, _]`(tfun, farg, aarg, barg) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.`App[_[_[_], _]]`(_, _) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not an assignable *")))
            case Term.TypeLevel.Var.`UserDefinedType`(nme, impl) => Right(Term.TypeLevel.Var.`UserDefinedType`(nme, Some(r.tpe)))
          )
          d <- StateT.pure(TypeDef.`Alias`[T](0, t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield `Type`[T](t)

      @scala.annotation.targetName("*->*")
      def :=[G[_], A](
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.tpe match
            case Term.TypeLevel.App.`App[_]`(tfun, aarg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_[_]]`(tfun, farg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_[_], _, _]`(tfun, farg, aarg, barg) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.`App[_[_[_], _]]`(_, _) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not an assignable *->*")))
            case Term.TypeLevel.Var.UserDefinedType(nme, impl) => Right(Term.TypeLevel.Var.`UserDefinedType[_]`(nme, Some(r.tpe)))
          )
          d <- StateT.pure(TypeDef.`Alias[_]=>>`[G](0, t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield `Type[_]`(t)

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]])
      def :=(
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]] =
        for
          (s, l) <- StateT.liftF(lhs.runEmpty)
          r <- StateT.liftF(rhs.runEmptyA)

          // d <- StateT.liftF(l.value )
          t <- StateT.liftF(l.value match
            case App1(fun, arg, tpe)  => Left(List(Error("Not an assignable *")))
            case App2(fun, arg, arg2, tpe) => Left(List(Error("Not an assignable *")))
            case AppPure(fun, arg, tpe) => Left(List(Error("Not an assignable *")))
            case AppVargs(fun, tpe, vargs*) => Left(List(Error("Not an assignable *")))
            case Dot0(fun, arg1, tpe) => Left(List(Error("Not an assignable *")))
            case Dot1(fun, arg1, arg2, tpe) => Left(List(Error("Not an assignable *")))
            case Dotless(fun, arg1, arg2, tpe) => Left(List(Error("Not an assignable *")))
            case ForComp(gens, ret, tpe) => Left(List(Error("Not an assignable *")))
            case Lam1(a, b, tpe) => Left(List(Error("Not an assignable *")))
            case Lam2(a1, a2, c, tpe) => Left(List(Error("Not an assignable *")))
            case BooleanLiteral(tpe, b) => Left(List(Error("Not an assignable *")))
            case IntLiteral(tpe, i) => Left(List(Error("Not an assignable *")))
            case StringLiteral(tpe, s) => Left(List(Error("Not an assignable *")))
            case UnitLiteral(tpe, u) => Left(List(Error("Not an assignable *")))
            case UserDefinedObject(nme, tpe, parent, body) => Left(List(Error("Not an assignable *")))
            case UserDefinedValue(nme, tpe, impl) => Right(Term.ValueLevel.Var.UserDefinedValue(nme, tpe, Some(r.value)))
           
          )
          
            // case Term.TypeLevel.App.`App[_]`(tfun, targ) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_[_]]`(tfun, farg) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_[_], _, _]`(tfun, farg, aarg, barg) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.`App[_[_[_], _]]`(_, _) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not an assignable *")))
            // case Term.TypeLevel.Var.`UserDefinedType`(nme, impl) => Right(Term.TypeLevel.Var.`UserDefinedType`(nme, Some(r.tpe)))
          // )
          (d, v) <- StateT.liftF(s._2.headOption.fold(Left(List(Error("Not an assignable *"))))(s => s match
            case `case class`(indent, caseclass) =>  Left(List(Error("Not an assignable *")))
            case `extension`(indent, extension) => Left(List(Error("Not an assignable *")))
            case `object`(indent, obj) => Left(List(Error("Not an assignable *")))
            case dc10.scala.Statement.`package`(indent, pkg) => Left(List(Error("Not an assignable *")))
            case `trait`(indent, t) => Left(List(Error("Not an assignable *")))
            case `trait[_]`(indent, tparam, t) => Left(List(Error("Not an assignable *")))
            case `trait[_[_]]`(indent, tparam, t) => Left(List(Error("Not an assignable *")))
            case `trait[_[_], _]`(indent, tparamF, tparamA, t) => Left(List(Error("Not an assignable *")))
            case `Alias`(indent, tpe) => Left(List(Error("Not an assignable *")))
            case `Alias[_]`(indent, tparam, tpe) => Left(List(Error("Not an assignable *")))
            case `Alias[_]=>>`(indent, tpe) => Left(List(Error("Not an assignable *")))
            case `Alias[_[_]]`(indent, tparam, tpe) => Left(List(Error("Not an assignable *")))
            case `Alias[_[_], _]`(indent, tparamF, tparamA, tpe) => Left(List(Error("Not an assignable *")))
            case `Match`(indent, tpe, rhs) => Left(List(Error("Not an assignable *")))
            case d@`def`(indent, arg, impl, tpe, value) => if impl.isEmpty then Left(List(Error("Not an assignable *"))) else Right((`def`(indent, arg, Some(t), t.tpe, UserDefinedValue(value.nme, t.tpe, Some(t))), UserDefinedValue(value.nme, t.tpe, Some(t))))
            case d@`def[_]`(indent, tparam, impl, value) => if impl.isEmpty then Left(List(Error("Not an assignable *"))) else Right((`def[_]`(indent, tparam, Some(t),  UserDefinedValue(value.nme, t.tpe, Some(t))), UserDefinedValue(value.nme, t.tpe, Some(t))))
            case d@`def[_[_]]`(indent, tparam, impl, value) => if impl.isEmpty then Left(List(Error("Not an assignable *"))) else Right((`def[_[_]]`(indent, tparam, Some(t), UserDefinedValue(value.nme, t.tpe, Some(t))), UserDefinedValue(value.nme, t.tpe, Some(t))))
            case d@`def[_[_], _]`(indent, tparamf, tparama, impl, value) => if impl.isEmpty then Left(List(Error("Not an assignable *"))) else Right((`def[_[_], _]`(indent, tparamf, tparama, Some(t),  UserDefinedValue(value.nme, t.tpe, Some(t))), UserDefinedValue(value.nme, t.tpe, Some(t))))
            case Fld(indent, value) => Left(List(Error("Not an assignable *")))
            case Gen(indent, value, impl) => Left(List(Error("Not an assignable *")))
            case `val`(indent, value, tpe) => if value.impl.isEmpty then Left(List(Error("Not an assignable *"))) else Left(List(Error("Not an assignable *")))
            case `Type`(tpe) => Left(List(Error("Not an assignable *")))
            case `Type[_]`(tpe) => Left(List(Error("Not an assignable *")))
            case `Type[_[_]]`(tpe) => Left(List(Error("Not an assignable *")))
            case `Type[_, _]`(tpe) => Left(List(Error("Not an assignable *")))
            case `Type[_[_], _]`(tpe) => Left(List(Error("Not an assignable *")))
            case `Type[_[_], _, _]`(tpe) => Left(List(Error("Not an assignable *")))
            case `Value`(value) => Left(List(Error("Not an assignable *")))
            case `Value[_[_], _]`(value) => Left(List(Error("Not an assignable *")))
          ))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield `Value`[T](v)

    @scala.annotation.targetName("0")
    def DEF[T](
      nme: String, 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, None, None, t.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("0*")
    def DEF[A, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      tpe: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def[_]`(0, a.tpe, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("0*->*->*")
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      tparam2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[B]],
      tpe: (`Type`[A], `Type`[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, None, None, t.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("0*->*")
    def DEF[G[_], T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
      tpe: `Type[_]`[G] => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        f <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def[_[_]]`(0, f.tpe, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)
      
    @scala.annotation.targetName("0(*->*)->*->*")
    def DEF[G[_], A, T](
      nme: String,
      tparamf: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
      tparama: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      tpe: (`Type[_]`[G], `Type`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        f <- StateT.liftF(tparamf.runEmptyA)
        a <- StateT.liftF(tparama.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f, a).runEmpty)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def[_[_], _]`(0, f.tpe, a.tpe, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]], 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]](`Type`[A]((a.value.tpe))) ==> tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, Some(a.value), None, r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]],
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: Value[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]](`Type`[A]((a.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]](a) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[A => T](nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, Some(a.value), Some(i.value), r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    def DEF[A, B, T](
      nme: String,
      arg1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]],
      arg2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[B]],
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: (Value[A], Value[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[(A, B) => T]] =
      for
        a <- StateT.liftF(arg1.runEmptyA)
        b <- StateT.liftF(arg2.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        i <- impl(a, b)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), (`Type`[A], `Type`[B])]((`Type`[A](a.value.tpe), `Type`[B](b.value.tpe))) ==> tpe
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), (`Value`[A], `Value`[B])]((a, b)) ==> impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(f.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`def`(0, Some(a.value), Some(i.value), r.tpe, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value(v)

    @scala.annotation.targetName("*")
    def TYPE[T](nme: String): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
      for
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.Var.`UserDefinedType`[T]](Term.TypeLevel.Var.`UserDefinedType`(nme, None))
        d <- StateT.pure(TypeDef.`Alias`[T](0, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Type(t)

    @scala.annotation.targetName("*->*")
    def TYPE[G[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_]`(0, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_]`(t)

    @scala.annotation.targetName("(*->*)->*")
    def TYPE[G[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_[_]]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_[_]]`(0, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_[_]]`(t)

    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      targA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[G]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType[_[_], _]`[G](nme, None))
        d <- StateT.pure(TypeDef.`Alias[_[_], _]`[G, H, A](0, f.tpe, a.tpe, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type[_[_], _]`(t)

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        t <- tpe
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue[T](nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`val`(0, v, t.tpe))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(v)

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]], 
      impl: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        i <- impl
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, Some(i.value)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.`val`(0, v, t.tpe))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Value[T](v)

    given `refT`[T]: Conversion[`Type`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T]]] =
      t => StateT.pure(t)

    given `refT[_]`[T[_]]: Conversion[`Type[_]`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_]]`[T[_[_]]]: Conversion[`Type[_[_]]`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[T]]] =
      t => StateT.pure(t)

    given `refT[_[_], _]`[T[_[_], _]]: Conversion[`Type[_[_], _]`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[T]]] =
      t => StateT.pure(t)

    given refV[T]: Conversion[`Value`[T], StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]]] =
      v => StateT.pure(v)