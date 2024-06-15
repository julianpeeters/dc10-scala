package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.ext
import dc10.scala.dsl.{OPTION}
import dc10.scala.{Error, ErrorF, LibDep, Statement}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`}
import dc10.scala.Statement.ValueExpr.{`Value`}
import dc10.scala.Symbol.{Extension, Term}
import org.tpolecat.sourcepos.SourcePos

trait Functions[F[_]]:

  extension [A, B] (domain: F[`Type`[A]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[`Type`[B]]): F[`Type`[A => B]]

  extension [A, B, C] (domain: F[(`Type`[A], `Type`[B])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[`Type`[C]]): F[`Type`[(A, B) => C]]

  extension [A, B] (fa: F[Value[A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: Value[A] => F[Value[B]]): F[Value[A => B]]

  extension [A, B, C] (fa: F[(Value[A], Value[B])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (Value[A], Value[B]) => F[Value[C]]): F[Value[(A, B) => C]]

  extension [G[_], A] (fa: F[`Type`[A]])
    @scala.annotation.targetName("tLam1")
    def ==>>(codomain: `Type`[A] => F[`Type`[G[A]]]): F[`Type[_]`[G]]

  def EXT[G[_], B](func: F[G[B]])(using sp: SourcePos): F[G[B]]

  @scala.annotation.targetName("forOption")
  def FOR[A](f: F[Value[A]])(using sp: SourcePos): F[Value[Option[A]]]

  extension [G[_], A] (nme: String)
    def <--(ff: F[Value[G[A]]])(using sp: SourcePos): F[Value[A]]
  
  // @scala.annotation.targetName("matchT1")
  // def MATCHTYPES[T[_], A, B](nme: String, arg: F[`Type`[A]], cases: `Type`[A] => F[B])(using sp: SourcePos): F[`Type`[T[A]]]
  
  def CASE[A, B](f: F[`Type`[A => B]])(using sp: SourcePos): F[Unit]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A]]
    with Applications.Mixins:
 
    extension [A, B] (domain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A => B]] =
        for
          a <- domain
          b <- codomain
          t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[A => B]](
            Term.TypeLevel.App.Infix(
              Term.TypeLevel.Var.`UserDefinedType[_, _]`("=>", None),
              a.tpe,
              b.tpe,
            )
          )
        yield Type(t)

    extension [A, B, C] (domain: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Type`[A], `Type`[B])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[C]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[(A, B) => C]] =
        for
          a <- domain
          b <- codomain
          f <- domain ==> codomain
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[(A, B) => C]](
            Term.TypeLevel.App.Infix2(
              Term.TypeLevel.Var.`UserDefinedType[_, _, _]`("=>", None),
              a._1.tpe,
              a._2.tpe,
              b.tpe,
            )
          )
    
        yield Type(v)

    extension [A, B] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: Value[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => B]] =
        for
          a <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Value[A]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[A => B]](
            Term.TypeLevel.App.Infix(Term.TypeLevel.Var.`UserDefinedType[_, _]`("=>", None), a.value.tpe, b.value.tpe)
          )
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[A => B]](Term.ValueLevel.Lam.Lam1(a.value, b.value, t))
        yield Value(v)

    extension [A, B, C] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), (Value[A], Value[B])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (Value[A], Value[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Value[C]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[(A, B) => C]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[(A, B) => C]](
            Term.TypeLevel.App.`App[_, _, _]`(
              Term.TypeLevel.Var.`UserDefinedType[_, _, _]`("=>", None),
              a._1.value.tpe,
              a._2.value.tpe,
              b.value.tpe)
          )
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[(A, B) => C]](Term.ValueLevel.Lam.Lam2(a._1.value, a._2.value, b.value, t))
        yield Value(v)

    extension [G[_], A] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]])
      @scala.annotation.targetName("tLam1")
      def ==>>(
        codomain: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[A]]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- codomain(a)
          t <- StateT.pure(Term.TypeLevel.Lam.Lam[G, A](a.tpe, b.tpe))
        yield `Type[_]`(t)

    def EXT[G[_], B](
      func: StateT[ErrorF, (Set[LibDep], List[Statement]), G[B]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), G[B]] =
      for
        ((ds, ms), f) <- StateT.liftF(func.runEmpty)
        e <- StateT.liftF(ms match
          case arg1 :: methods => Right(Extension(arg1, methods))
          case Nil => Either.left[List[Error], Extension](List(Error(s"${sp.file}:${sp.line}\nToo many extension arguments")))
        )
        d <- StateT.pure(Statement.extension(0, sp, e))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield f

    @scala.annotation.targetName("forOption")
    def FOR[A](f: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]])(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[Option[A]]] =
      for
        ((d, l), a) <- StateT.liftF(f.runEmpty)
        t <- OPTION(StateT.pure(Type(a.value.tpe)))
        v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[Option[A]]](Term.ValueLevel.App.ForComp(l, a.value, t.tpe))
      yield Value(v)

    extension [G[_], A] (nme: String)
      def <--(
        ff: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[G[A]]]
      )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]] =
        for
          g <- ff
          t <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[A]](g.value.tpe match
            case dc10.scala.Symbol.Term.TypeLevel.App.`App[_]`(tfun, targ) => Right(targ)
            case dc10.scala.Symbol.Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.TypeLevel.App.Infix(tfun, ta, tb) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.TypeLevel.Var.`UserDefinedType`(nme, impl) => Left(List(Error("Not of kind *->*")))            
          )
          i <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[A]](g.value.findImpl.fold(Left(List(Error(""))))(i => i match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(fun, arg, tpe) => Right(arg.asInstanceOf[Term.ValueLevel.`*`[A]]) 
            case dc10.scala.Symbol.Term.ValueLevel.App.App2(fun, arg1, arg2, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.App.AppPure(fun, arg, tpe) => Right(arg)
            case dc10.scala.Symbol.Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.App.Dot0(fun, arg1, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.App.ForComp(l, r, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1(a, b, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral(tpe, b) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral(tpe, i) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral(tpe, s) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral(tpe, s) => Left(List(Error("Not of kind *->*")))
            case dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => Left(List(Error("Not of kind *->*")))
          ))
          v <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.Var.UserDefinedValue[A]](
                Right(Term.ValueLevel.Var.UserDefinedValue(nme, t, Some(i))))
          d <- StateT.pure(Statement.ValueDef.Gen(0, sp, v, g.value))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield Value(v)

    // @scala.annotation.targetName("matchT1")
    // def MATCHTYPES[T[_], A, B](
    //   nme: String,
    //   arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
    //   cases: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), B]
    // )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[A]]] =
    //   for
    //     a <- StateT.liftF(arg.runEmptyA)
    //     (d, l) <- StateT.liftF(cases(a).runEmptyS)
    //     c <- StateT.liftF(l.map(s => s match
    //       case Type(tpe) => tpe match
    //         case Infix(tfun, ta, tb) => Right(Infix(tfun, ta, tb))
    //         case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected function but found ${tpe}")))
    //       case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected cases but found ${a.tpe}")))
    //     ).sequence)
    //     f <- StateT.pure(TypeLevel.Var.`UserDefinedType[_]`(nme, None))
    //     t <- StateT.liftF(a.tpe match
    //       case u@TypeLevel.Var.`UserDefinedType`(_, _) => Right(Term.TypeLevel.App.`App[_]`(f, a.tpe 
    //       ))
    //       case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected user-defined type but found ${a.tpe}")))
    //     )
    //     d <- StateT.liftF(c.toNel.fold(Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected at least one case"))))(nel => Right(TypeDef.Match(0, sp, t, nel))))
    //     _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
    //   yield Type(t)

    def CASE[A, B](f: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A => B]])(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), Unit] =
      for
        a <- f
        _ <- f.modify(d => (d._1, d._2 :+ a))
      yield ()