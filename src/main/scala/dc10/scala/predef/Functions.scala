package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{ExtensionDef, TypeDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{Extension, Term}
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import dc10.scala.Symbol.Term.TypeLevel.App.Infix
import org.tpolecat.sourcepos.SourcePos
import dc10.scala.Statement.TypeDef.Match
import dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue

trait Functions[F[_]]:

  extension [A, B, Y, Z] (domain: F[TypeExpr[A, Y]])
    @scala.annotation.targetName("fun1T")
    def ==>(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[A => B, Unit]]

  extension [Z, A, B] (domain: F[(TypeExpr[A, Z], TypeExpr[A, Z])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[(A, A) => B, Z]]

  extension [A, B, X, Y] (fa: F[ValueExpr[A, Y]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: ValueExpr[A, Y] => F[ValueExpr[B, X]]): F[ValueExpr[A => B, Unit]]

  extension [A, B] (fa: F[(ValueExpr[A, Unit], ValueExpr[A, Unit])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => F[ValueExpr[B, Unit]]): F[ValueExpr[(A, A) => B, Unit]]

  def FOR[G[_], A, X, Y](f: F[ValueExpr[G[A], (X, Y)]])(using sp: SourcePos): F[ValueExpr[G[A], (X, Y)]]

  extension [G[_], A, X, Y] (nme: String)
    def <--(ff: F[ValueExpr[G[A], (X,Y)]]): F[ValueExpr[A, Y]]
  
  @scala.annotation.targetName("yieldOption")
  def YIELD[B, Y, Z](arg: ValueExpr[B, Y])(using sp: SourcePos): ValueExpr[Option[B], (Unit, Y)]

  @scala.annotation.targetName("matchT1")
  def MATCHTYPES[T[_], A, B, Y](nme: String, arg: F[TypeExpr[A, Y]], cases: TypeExpr[A, Y] => F[B])(using sp: SourcePos): F[TypeExpr[T[A], Y]]
  
  def CASE[A, B, X, Z](f: F[TypeExpr[A => B, Z]])(using sp: SourcePos): F[Unit]

object Functions:

  trait Mixins extends Functions[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B, Y, Z] (domain: StateT[ErrorF, List[Statement], TypeExpr[A, Y]])
      @scala.annotation.targetName("fun1T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], TypeExpr[A => B, Unit]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Unit]](
            Term.TypeLevel.App.Infix(
              None,
              Term.TypeLevel.Lam.Function1Type(None, ()),
              a.tpe,
              b.tpe,
              ()
            )
          )
  
        yield TypeExpr(v)

    extension [Z, A, B] (domain: StateT[ErrorF, List[Statement], (TypeExpr[A, Z], TypeExpr[A, Z])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], TypeExpr[(A, A) => B, Z]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B, Z]](
            Term.TypeLevel.App.App3(
              None,
              Term.TypeLevel.Lam.Function2Type(None, b.tpe.dep),
              a._1.tpe,
              a._2.tpe,
              b.tpe,
              b.tpe.dep
            )
          )
    
        yield TypeExpr(v)

    extension [A, B, X, Y] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: ValueExpr[A, Y] => StateT[ErrorF, List[Statement], ValueExpr[B, X]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B, Unit]] =
        for
          a <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Y]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[A => B, Unit]](
            Term.TypeLevel.App.App2(None, Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep), a.value.tpe, b.value.tpe, ())
          )
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[A => B, Unit]](Term.ValueLevel.Lam.Lam1(None, a.value, b.value, t))
        yield ValueExpr(v)

    extension [A, B] (fa: StateT[ErrorF, List[Statement], (ValueExpr[A, Unit], ValueExpr[A, Unit])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (ValueExpr[A, Unit], ValueExpr[A, Unit]) => StateT[ErrorF, List[Statement], ValueExpr[B, Unit]]
      ): StateT[ErrorF, List[Statement], ValueExpr[(A, A) => B, Unit]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, List[Statement], TypeLevel[(A, A) => B, Unit]](Term.TypeLevel.App.App3(None, Term.TypeLevel.Lam.Function2Type(None, ()), a._1.value.tpe, a._2.value.tpe, b.value.tpe, ()))
          v <- StateT.pure[ErrorF, List[Statement], ValueLevel[(A, A) => B, Unit]](Term.ValueLevel.Lam.Lam2(None, a._1.value, a._2.value, b.value, t))
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

    def FOR[G[_], A, X, Y](f: StateT[ErrorF, List[Statement], ValueExpr[G[A], (X, Y)]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[G[A], (X, Y)]] =
      for
        (l, v) <- StateT.liftF(f.runEmpty)
        a <- StateT.liftF(v.value.findImpl.fold(Left(List(Error("Expected a pure application"))))(i => i match
            case App1(qnt, fun, arg, tpe) => ??? 
            case AppCtor1(qnt, tpe, arg) => ???
            case AppCtor2(qnt, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => Right(arg)
            case AppVargs(qnt, fun, tpe, vargs*) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case ForComp(qnt, gens, ret, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) => ???
          )
        )

        v <- StateT.pure(ValueExpr(ForComp(None, l, a, v.value.tpe)))
        // _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))

        // in this model of a for comprehension, we'll let the use define some generators, and write tiem into a
        // a model of the for, which is a type of value
      yield v

    extension [G[_], A, X, Y] (nme: String)
      def <--(ff: StateT[ErrorF, List[Statement], ValueExpr[G[A], (X, Y)]]): StateT[ErrorF, List[Statement], ValueExpr[A, Y]] =
        for
          g <- ff
          t <- StateT.liftF[ErrorF, List[Statement], TypeLevel[A, Y]](g.value.tpe match
            case dc10.scala.Symbol.Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => Right(targ.asInstanceOf[TypeLevel[A, Y]])
            case dc10.scala.Symbol.Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function1Type(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Lam.Function2Type(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.BooleanType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.IntType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.StringType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.ListType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.OptionType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.SomeType(qnt, dep) => ???
            case dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => ???
          )
          i <- StateT.liftF[ErrorF, List[Statement], ValueLevel[A, Y]](g.value.findImpl.fold(Left(List(Error(""))))(i => i match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => ??? 
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2(qnt, tpe, arg1, arg2) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Right(arg.asInstanceOf[ValueLevel[A, Y]])
            case dc10.scala.Symbol.Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp(qnt, l, r, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2(qnt, a1, a2, c, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor(qnt, tpe) => ???
            case dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => ???
          )
          )
          
          
          // a <- fa

            //   for
        // t <- tpe
        // i <- impl
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel.Var.UserDefinedValue[A, Y]](
            // if (t.dep == i.value.tpe.dep)
              // then 
                Right(Term.ValueLevel.Var.UserDefinedValue(None, nme, t, Some(i))))
              // else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error: ${t.tpe.dep} =/= ${i.value.tpe.dep}"))))
          d <- StateT.pure(ValueDef.Gen(0, v, g.value))
          _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))

        yield ValueExpr(v)

    // def VAL[Z, T](
    //   nme: String,
    //   tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Z]], 
    //   impl: StateT[ErrorF, List[Statement], ValueExpr[T, Z]]
    // )(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[T, Z]] =
    //   for
    //     t <- tpe
    //     i <- impl
    //     v <- StateT.liftF(
    //       if (t.tpe.dep == i.value.tpe.dep)
    //         then Right(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, Some(i.value)))
    //         else Left(List(Error(s"${sp.file}:${sp.line}\nDependent type error: ${t.tpe.dep} =/= ${i.value.tpe.dep}"))))
    //     d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Val(0, v))
    //     _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
    //   yield ValueExpr[T, Z](v)


    // def YIELD[B, Y, Z](f: F[ValueExpr[B, Y]])(using sp: SourcePos): F[ValueExpr[Option[B], (Unit, Y)]]

    @scala.annotation.targetName("yieldOption")
    def YIELD[B, Y, Z](arg: ValueExpr[B, Y])(using sp: SourcePos): ValueExpr[Option[B], (Unit, Y)] =
      // for
      //   o <- StateT.pure(ValueExpr(Term.ValueLevel.Var.OptionCtor(None, Term.TypeLevel.Var.OptionType[Option[B], Unit](None, ()))))
      //   a <- arg
      //   t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[B], (Unit, Y)]](Term.TypeLevel.App.App1(
      //     None,
      //     Term.TypeLevel.Var.OptionType(None, ()),
      //     a.value.tpe,
      //     ((), a.value.tpe.dep)
      //   ))
      // yield 
      ValueExpr(Term.ValueLevel.App.AppPure(None, Term.ValueLevel.Var.OptionCtor(None, Term.TypeLevel.Var.OptionType[Option[B], Unit](None, ())), arg.value, Term.TypeLevel.App.App1(
          None,
          Term.TypeLevel.Var.OptionType(None, ()),
          arg.value.tpe,
          ((), arg.value.tpe.dep)
        )))
      // Option(f)
      // for
        // b <- f
      //   o <- Option(f)
      // yield o


   
    // def Option1[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]] =
    //   StateT.pure(ValueExpr(Term.ValueLevel.Var.OptionCtor(None, Term.TypeLevel.Var.OptionType(None, ()))))
    
    // extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]])
    //   @scala.annotation.targetName("appVO")
    //   def apply1[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A], (Unit, Z)]] =
    //     for
    //       o <- option
    //       a <- arg
    //       t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[A], (Unit, Z)]](Term.TypeLevel.App.App1(
    //         None,
    //         Term.TypeLevel.Var.OptionType(None, ()),
    //         a.value.tpe,
    //         ((), a.value.tpe.dep)
    //       ))
    //     yield ValueExpr(Term.ValueLevel.App.AppPure(None, o.value, a.value, t))










    @scala.annotation.targetName("matchT1")
    def MATCHTYPES[T[_], A, B, Y](
      nme: String,
      arg: StateT[ErrorF, List[Statement], TypeExpr[A, Y]],
      cases: TypeExpr[A, Y] => StateT[ErrorF, List[Statement], B]
    )(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[T[A], Y]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        l <- StateT.liftF(cases(a).runEmptyS)
        c <- StateT.liftF(l.map(s => s match
          case TypeExpr(tpe) => tpe match
            case Infix(qnt, tfun, ta, tb, dep) => Right(Infix(qnt, tfun, ta, tb, dep))
            case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected function but found ${tpe}")))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected cases but found ${a.tpe}")))
        ).sequence)
        f <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel.Var.UserDefinedType[T[A], Unit]](Term.TypeLevel.Var.UserDefinedType(None, nme, None, ()))
        t <- StateT.liftF(a.tpe match
          case u@Term.TypeLevel.Var.UserDefinedType(_, _, _, _) => Right(Term.TypeLevel.App.App1(None, f, a.tpe, (a.tpe.dep)))
          case _ => Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected user-defined type but found ${a.tpe}")))
        )
        d <- StateT.liftF(c.toNel.fold(Left(List(Error(s"${sp.file}:${sp.line}\nMatch types error: expected at least one case"))))(nel => Right(TypeDef.Match(0, t, nel))))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield TypeExpr(t)

    def CASE[A, B, X, Z](f: StateT[ErrorF, List[Statement], TypeExpr[A => B, Z]])(using sp: SourcePos): StateT[ErrorF, List[Statement], Unit] =
      for
        a <- f
        _ <- f.modify(d => d :+ a)
      yield ()