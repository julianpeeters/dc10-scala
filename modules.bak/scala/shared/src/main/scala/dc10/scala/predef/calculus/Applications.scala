package dc10.scala.predef.calculus

import cats.data.StateT
import cats.implicits.given
import dc10.scala.*
import dc10.scala.internal.extract.unpure
import dc10.scala.internal.substitute.sub
import dc10.scala.internal.indent.getIndent

trait Applications[F[_]]:

  extension [T[_], `T.*`[t] <: `Type.Expr: *`[t], `T.*→*`[t[_]] <: `Type: *→*`[t]] (function: F[`Type.Var: *→*`[T]])
    @scala.annotation.targetName("F[A]")
    def apply[A](args: F[`T.*`[A]]): F[`Type.App[_]`[T, A]]

  extension [T[_[_]]] (tfunction: F[`Type: (*→*)→*`[T]])
    @scala.annotation.targetName("F[G]")
    def apply[G[_]](farg: F[`Type: *→*`[G]]): F[`Type.App[_[_]]`[T, G]]

  extension [T[_,_], `T.*→*→*`[t[_, _]] <: `Type: *→*→*`[t]] (tfunction: F[`T.*→*→*`[T]])
    @scala.annotation.targetName("F[A, B]")
    def apply[
      // `T1.*`[t] <: `Type.Expr: *`[t],
      // `T2.*`[t] <: `Type.Expr: *`[t],
      A,
      B
    ](fta: F[`Type.Expr: *`[A]], ftb: F[`Type.Expr: *`[B]]): F[`Type.App[_, _]`[T, A, B]]

  extension [T[_[_],_]] (tfunction: F[`Type: (*→*)→*→*`[T]])
    @scala.annotation.targetName("F[G, A]")
    def apply[G[_], A](farg: F[`Type: *→*`[G]], aarg: F[`Type.Expr: *`[A]]): F[`Type.App[_[_], _]`[T, G, A]]

  extension [T[_[_],_,_]] (tfunction: F[`Type.(*→*)→*→*→*`[T]])
    @scala.annotation.targetName("F[G, A, B]")
    def apply[G[_], A, B](farg: F[`Type: *→*`[G]], aarg: F[`Type.Expr: *`[A]], barg: F[`Type.Expr: *`[B]]): F[`Type.Expr: *`[T[G, A, B]]]

  extension [`V.*`[t] <: `Value.Expr: *`[t], A, B] (function: F[`Value.Expr: *`[A => B]])
    @scala.annotation.targetName("A => B")
    def apply(args: F[`V.*`[A]]): F[`Value.Expr: *`[B]]

  extension [A, B] (function: F[`Value.Expr: *`[List[A] => B]])
    @scala.annotation.targetName("List[A] => B")
    def apply(vargs: F[`Value.Expr: *`[A]]*): F[`Value.App.Vargs: *`[A, B]]


  // `Type.Lam: *→*`[G, A]

  // extension [
  //   G[_],
  //   `T.*`[t] <: `Type.Expr: *`[t],
  //   `V.*`[t] <: `Value.Expr: *`[t]
  // ] (function: F[`Value.*→*`[[A] =>> A => G[A]]])
  //   @scala.annotation.targetName("pure")
  //   def apply[A](arg: F[`V.*`[A]]): F[`Value.App.1: *`[A, G[A], `T.*`, `V.*`]]

  extension [G[_]] (function: F[`Value.*→*`[[A] =>> List[A] => G[A]]])
    @scala.annotation.targetName("pureVargs")
    def apply[A](vargs: F[`Value.Expr: *`[A]]*): F[`Value.Expr: *`[G[A]]]

  extension [A, B] (arg1: F[`Value.Expr: *`[A]])
    def DOT(func: F[`Value.Expr: *`[A => B]])(arg2: F[`Value.Expr: *`[B]]): F[`Value.Expr: *`[B]]

object Applications:

  extension [T[_]] (f: `Type: *→*`[[A] =>> T[A]])
    def apply[A](a: `Type.Expr: *`[A]): Either[List[Error], `Type.Expr: *`[T[A]]] =
      f match
        case `Type.Lam: *→*`(in, domain, codomain) => codomain.asInstanceOf[`Type.Expr: *`[T[A]]].sub(a)
        case `Type.Var: *→*`(in, nme, impl, c) => impl.fold(Right(`Type.App[_]`(0, f, a)))(i => i.apply(a))
        // case `Type.Var2[_]`(in, nme, impl, _, _) => impl.fold[Either[List[Error], `Type.Expr: *`[T[A]]]](Left(List(Error(s"unimplemented type does not support substitution ${f}"))))(_ => ???)//i.applyType(a))

  extension [T[_]] (f: `Type: *→*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply [A] =>> A => T[A]")
    def applyType[A, `T.*`[t] <: `Type.Expr: *`[t]](a: `T.*`[A]): Either[List[Error], `Type.Expr: *`[A => T[A]]] =
      f match
        case `Type.Lam: *→*`(in, domain, codomain) => codomain.asInstanceOf[`Type.Expr: *`[A => T[A]]].sub(a)
        case `Type.Var: *→*`(in, nme, impl, c) => ???

  extension [T[_]] (f: `Type: *→*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply [A] =>> List[A] => T[A]")
    def applyType[A](a: `Type.Expr: *`[A]): Either[List[Error], `Type.Expr: *`[List[A] => T[A]]] =
      f match
        case `Type.Lam: *→*`(in, domain, codomain) => codomain.asInstanceOf[`Type.Expr: *`[List[A] => T[A]]].sub(a)
        case `Type.Var: *→*`(in, nme, impl, c) => ???

  extension [T[_]] (v: `Value.*→*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply T [A] =>> A => T[A]")
    def applyValue[A](targ: `Type.Expr: *`[A]): Either[List[Error], `Value.Expr: *`[A => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.App.0: *→*`(v.getIndent, v, targ, r))
    // @scala.annotation.targetName("apply V [A] =>> A => T[A]")
    // // def applyValue[A](a: `Value.Expr: *`[A]): Either[List[Error], `Value.Expr: *`[T[A]]] =
    // def applyValue[
    //   A,
    //   `T.*`[t] <: `Type.Expr: *`[t],
    //   `V.*`[t] <: `Value.Expr: *`[t]
    // ](a: `V.*`[A]): Either[List[Error], `Value.App.1: *`[A, T[A], `T.*`, `V.*`]] =
    //   for
    //     c <- v.applyValue(a.tpe)
    //     t <- v.tpe.applyType(a.tpe)
    //     r <- t.unpure
    //   yield `Value.App.1: *`(v.getIndent, c, a, r)

  extension [T[_]] (v: `Value.*→*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply T [A] =>> List[A] => T[A]")
    def applyValue[A](targ: `Type.Expr: *`[A]): Either[List[Error], `Value.Expr: *`[List[A] => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.App.0: *→*`(v.getIndent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> List[A] => T[A]")
    def applyValue[A](a: `Value.Expr: *`[A]*): Either[List[Error], `Value.Expr: *`[T[A]]] =
      for
        x <- a.toList.headOption.fold(Right(`Type.Bot: *`(v.getIndent)))(a => Right(a.tpe))
        c <- v.applyValue(x)
        t <- v.tpe.applyType(x)
        r <- t.unpure
      yield `Value.App.Vargs: *`[A, T[A]](v.getIndent, c, r, a*)

  trait Mixins extends Applications[StateT[ErrorF, Γ, _]]:

    extension [T[_], `T.*`[t] <: `Type.Expr: *`[t], `T.*→*`[t[_]] <: `Type: *→*`[t]] (tfunction: StateT[ErrorF, Γ, `Type.Var: *→*`[T]])
      @scala.annotation.targetName("F[A]")
      def apply[A](
        args: StateT[ErrorF, Γ, `T.*`[A]]
      ): StateT[ErrorF, Γ, `Type.App[_]`[T, A]] =
        for
          f <- tfunction
          a <- args
        yield `Type.App[_]`(0, f, a)

    extension [T[_[_]]] (tfunction: StateT[ErrorF, Γ, `Type: (*→*)→*`[T]])
      @scala.annotation.targetName("F[G]")
      def apply[G[_]](
        farg: StateT[ErrorF, Γ, `Type: *→*`[G]],
      ): StateT[ErrorF, Γ, `Type.App[_[_]]`[T, G]] =
        for
          t <- tfunction
          f <- farg
        yield `Type.App[_[_]]`(0, t, f)

    extension [T[_,_], `T.*→*→*`[t[_, _]] <: `Type: *→*→*`[t]] (tfunction: StateT[ErrorF, Γ, `T.*→*→*`[T]])
      @scala.annotation.targetName("F[A, B]")
      def apply[
        // `T1.*`[t] <: `Type.Expr: *`[t],
        // `T2.*`[t] <: `Type.Expr: *`[t],
        A,
        B
      ](
        fta: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
        ftb: StateT[ErrorF, Γ, `Type.Expr: *`[B]]
      ): StateT[ErrorF, Γ, `Type.App[_, _]`[T, A, B]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield `Type.App[_, _]`(0, f, a, b)

    extension [T[_[_], _]] (tfunction: StateT[ErrorF, Γ, `Type: (*→*)→*→*`[T]])
      @scala.annotation.targetName("F[G, A]")
      def apply[G[_], A](
        farg: StateT[ErrorF, Γ, `Type: *→*`[G]],
        aarg: StateT[ErrorF, Γ, `Type.Expr: *`[A]]
      ): StateT[ErrorF, Γ, `Type.App[_[_], _]`[T, G, A]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
        yield `Type.App[_[_], _]`(0, t, f, a)

    extension [T[_,_,_]] (tfunction: StateT[ErrorF, Γ, `Type.*→*→*→*`[T]])
      @scala.annotation.targetName("F[A, B, C]")
      def apply[A, B, C](
        fta: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
        ftb: StateT[ErrorF, Γ, `Type.Expr: *`[B]],
        ftc: StateT[ErrorF, Γ, `Type.Expr: *`[C]]
      ): StateT[ErrorF, Γ, `Type.Expr: *`[T[A, B, C]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
          c <- ftc
        yield `Type.App[_, _, _]`(0, f, a, b, c)

    extension [T[_[_], _, _]] (tfunction: StateT[ErrorF, Γ, `Type.(*→*)→*→*→*`[T]])
      @scala.annotation.targetName("F[G, A, B]")
      def apply[G[_], A, B](
        farg: StateT[ErrorF, Γ, `Type: *→*`[G]],
        aarg: StateT[ErrorF, Γ, `Type.Expr: *`[A]],
        barg: StateT[ErrorF, Γ, `Type.Expr: *`[B]]
      ): StateT[ErrorF, Γ, `Type.Expr: *`[T[G, A, B]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
          b <- barg
        yield `Type.App[_[_], _, _]`(0, t, f, a, b)

    extension [`V.*`[t] <: `Value.Expr: *`[t], A, B] (function: StateT[ErrorF, Γ, `Value.Expr: *`[A => B]])
      @scala.annotation.targetName("A => B")
      def apply(args: StateT[ErrorF, Γ, `V.*`[A]]): StateT[ErrorF, Γ, `Value.Expr: *`[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF(f.tpe.unpure)
        yield `Value.App.1: *`(0, f, a, t)

    extension [A, B] (function: StateT[ErrorF, Γ, `Value.Expr: *`[List[A] => B]])
      @scala.annotation.targetName("List[A] => B")
      def apply(vargs: StateT[ErrorF, Γ, `Value.Expr: *`[A]]*): StateT[ErrorF, Γ, `Value.App.Vargs: *`[A, B]] =
        for
          f <- function
          a <- vargs.toList.sequence
          t <- StateT.liftF(f.tpe.unpure)
        yield `Value.App.Vargs: *`(0, f, t, a*)


    // extension [
    //   G[_],
    //   `T.*`[t] <: `Type.Expr: *`[t],
    //   `V.*`[t] <: `Value.Expr: *`[t]
    //   // `V.*`[t] <: `Value.Expr: *`[t],
    //   // `V.*→*`[u[_]] <: `Value.*→*`[u]
    // ] (function: StateT[ErrorF, Γ, `Value.*→*`[[A] =>> A => G[A]]])
    //   @scala.annotation.targetName("pure")
    //   def apply[A](arg: StateT[ErrorF, Γ, `V.*`[A]]): StateT[ErrorF, Γ, `Value.App.1: *`[A, G[A], `T.*`, `V.*`]] =
        
    //     for
    //       a <- arg
    //       f <- function
    //       m <- StateT.liftF(f.applyValue(a))
    //     yield m

    extension [G[_]] (function: StateT[ErrorF, Γ, `Value.*→*`[[A] =>> List[A] => G[A]]])
      @scala.annotation.targetName("pureVargs")
      def apply[A](vargs: StateT[ErrorF, Γ, `Value.Expr: *`[A]]*): StateT[ErrorF, Γ, `Value.Expr: *`[G[A]]] =
        for
          as <- vargs.toList.sequence
          f <- function
          g <- StateT.liftF(f.applyValue(as*))
        yield g
    
    extension [A, B] (arg1: StateT[ErrorF, Γ, `Value.Expr: *`[A]])
      def DOT(func: StateT[ErrorF, Γ, `Value.Expr: *`[A => B]])(arg2: StateT[ErrorF, Γ, `Value.Expr: *`[B]]): StateT[ErrorF, Γ, `Value.Expr: *`[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
        yield `Value.AppDot.1: *`(0, f, a1, a2, a2.tpe)