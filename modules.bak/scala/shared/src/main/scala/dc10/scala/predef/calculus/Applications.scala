package dc10.scala.predef.calculus

import cats.data.StateT
import cats.implicits.given
import dc10.scala.*
import dc10.scala.internal.extract.unpure
import dc10.scala.internal.substitute.sub
import dc10.scala.internal.indent.getIndent

trait Applications[F[_]]:

  extension [T[_], `T.x`[t] <: `Type: x`[t], `T.x_x`[t[_]] <: `Type: x_x`[t]] (function: F[`Type.Var: x_x`[T]])
    @scala.annotation.targetName("F[A]")
    def apply[A](args: F[`T.x`[A]]): F[`Type.App[_]`[T, A]]

  extension [T[_[_]]] (tfunction: F[`Type: lx_xl_x`[T]])
    @scala.annotation.targetName("F[G]")
    def apply[G[_]](farg: F[`Type: x_x`[G]]): F[`Type.App[_[_]]`[T, G]]

  extension [T[_,_], `T.x_x_x`[t[_, _]] <: `Type: x_x_x`[t]] (tfunction: F[`T.x_x_x`[T]])
    @scala.annotation.targetName("F[A, B]")
    def apply[
      // `T1.x`[t] <: `Type: x`[t],
      // `T2.x`[t] <: `Type: x`[t],
      A,
      B
    ](fta: F[`Type: x`[A]], ftb: F[`Type: x`[B]]): F[`Type.App[_, _]`[T, A, B]]

  extension [T[_[_],_]] (tfunction: F[`Type: lx_xl_x_x`[T]])
    @scala.annotation.targetName("F[G, A]")
    def apply[G[_], A](farg: F[`Type: x_x`[G]], aarg: F[`Type: x`[A]]): F[`Type.App[_[_], _]`[T, G, A]]

  extension [T[_[_],_,_]] (tfunction: F[`Type.lx_xl_x_x_x`[T]])
    @scala.annotation.targetName("F[G, A, B]")
    def apply[G[_], A, B](farg: F[`Type: x_x`[G]], aarg: F[`Type: x`[A]], barg: F[`Type: x`[B]]): F[`Type: x`[T[G, A, B]]]

  extension [`V.x`[t] <: `Value: x`[t], A, B] (function: F[`Value: x`[A => B]])
    @scala.annotation.targetName("A => B")
    def apply(args: F[`V.x`[A]]): F[`Value: x`[B]]

  extension [A, B] (function: F[`Value: x`[List[A] => B]])
    @scala.annotation.targetName("List[A] => B")
    def apply(vargs: F[`Value: x`[A]]xl: F[`Value.App.Vargs: x`[A, B]]


  // `Type.Lam: x_x`[G, A]

  // extension [
  //   G[_],
  //   `T.x`[t] <: `Type: x`[t],
  //   `V.x`[t] <: `Value: x`[t]
  // ] (function: F[`Value.x_x`[[A] =>> A => G[A]]])
  //   @scala.annotation.targetName("pure")
  //   def apply[A](arg: F[`V.x`[A]]): F[`Value.App.1: x`[A, G[A], `T.x`, `V.x`]]

  extension [G[_]] (function: F[`Value.x_x`[[A] =>> List[A] => G[A]]])
    @scala.annotation.targetName("pureVargs")
    def apply[A](vargs: F[`Value: x`[A]]xl: F[`Value: x`[G[A]]]

  extension [A, B] (arg1: F[`Value: x`[A]])
    def DOT(func: F[`Value: x`[A => B]])(arg2: F[`Value: x`[B]]): F[`Value: x`[B]]

object Applications:

  extension [T[_]] (f: `Type: x_x`[[A] =>> T[A]])
    def apply[A](a: `Type: x`[A]): Either[List[Error], `Type: x`[T[A]]] =
      f match
        case `Type.Lam: x_x`(in, domain, codomain) => codomain.asInstanceOf[`Type: x`[T[A]]].sub(a)
        case `Type.Var: x_x`(in, nme, impl, c) => impl.fold(Right(`Type.App[_]`(0, f, a)))(i => i.apply(a))
        // case `Type.Var2[_]`(in, nme, impl, _, _) => impl.fold[Either[List[Error], `Type: x`[T[A]]]](Left(List(Error(s"unimplemented type does not support substitution ${f}"))))(_ => ???)//i.applyType(a))

  extension [T[_]] (f: `Type: x_x`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply [A] =>> A => T[A]")
    def applyType[A, `T.x`[t] <: `Type: x`[t]](a: `T.x`[A]): Either[List[Error], `Type: x`[A => T[A]]] =
      f match
        case `Type.Lam: x_x`(in, domain, codomain) => codomain.asInstanceOf[`Type: x`[A => T[A]]].sub(a)
        case `Type.Var: x_x`(in, nme, impl, c) => ???

  extension [T[_]] (f: `Type: x_x`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply [A] =>> List[A] => T[A]")
    def applyType[A](a: `Type: x`[A]): Either[List[Error], `Type: x`[List[A] => T[A]]] =
      f match
        case `Type.Lam: x_x`(in, domain, codomain) => codomain.asInstanceOf[`Type: x`[List[A] => T[A]]].sub(a)
        case `Type.Var: x_x`(in, nme, impl, c) => ???

  extension [T[_]] (v: `Value.x_x`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply T [A] =>> A => T[A]")
    def applyValue[A](targ: `Type: x`[A]): Either[List[Error], `Value: x`[A => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.App.0: x_x`(v.getIndent, v, targ, r))
    // @scala.annotation.targetName("apply V [A] =>> A => T[A]")
    // // def applyValue[A](a: `Value: x`[A]): Either[List[Error], `Value: x`[T[A]]] =
    // def applyValue[
    //   A,
    //   `T.x`[t] <: `Type: x`[t],
    //   `V.x`[t] <: `Value: x`[t]
    // ](a: `V.x`[A]): Either[List[Error], `Value.App.1: x`[A, T[A], `T.x`, `V.x`]] =
    //   for
    //     c <- v.applyValue(a.tpe)
    //     t <- v.tpe.applyType(a.tpe)
    //     r <- t.unpure
    //   yield `Value.App.1: x`(v.getIndent, c, a, r)

  extension [T[_]] (v: `Value.x_x`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply T [A] =>> List[A] => T[A]")
    def applyValue[A](targ: `Type: x`[A]): Either[List[Error], `Value: x`[List[A] => T[A]]] =
      v.tpe.applyType(targ).map(r => `Value.App.0: x_x`(v.getIndent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> List[A] => T[A]")
    def applyValue[A](a: `Value: x`[A]xl: Either[List[Error], `Value: x`[T[A]]] =
      for
        x <- a.toList.headOption.fold(Right(`Type.Bot: x`(v.getIndent)))(a => Right(a.tpe))
        c <- v.applyValuelxl
        t <- v.tpe.applyTypelxl
        r <- t.unpure
      yield `Value.App.Vargs: x`[A, T[A]](v.getIndent, c, r, axl

  trait Mixins extends Applications[StateT[ErrorF, Γ, _]]:

    extension [T[_], `T.x`[t] <: `Type: x`[t], `T.x_x`[t[_]] <: `Type: x_x`[t]] (tfunction: StateT[ErrorF, Γ, `Type.Var: x_x`[T]])
      @scala.annotation.targetName("F[A]")
      def apply[A](
        args: StateT[ErrorF, Γ, `T.x`[A]]
      ): StateT[ErrorF, Γ, `Type.App[_]`[T, A]] =
        for
          f <- tfunction
          a <- args
        yield `Type.App[_]`(0, f, a)

    extension [T[_[_]]] (tfunction: StateT[ErrorF, Γ, `Type: lx_xl_x`[T]])
      @scala.annotation.targetName("F[G]")
      def apply[G[_]](
        farg: StateT[ErrorF, Γ, `Type: x_x`[G]],
      ): StateT[ErrorF, Γ, `Type.App[_[_]]`[T, G]] =
        for
          t <- tfunction
          f <- farg
        yield `Type.App[_[_]]`(0, t, f)

    extension [T[_,_], `T.x_x_x`[t[_, _]] <: `Type: x_x_x`[t]] (tfunction: StateT[ErrorF, Γ, `T.x_x_x`[T]])
      @scala.annotation.targetName("F[A, B]")
      def apply[
        // `T1.x`[t] <: `Type: x`[t],
        // `T2.x`[t] <: `Type: x`[t],
        A,
        B
      ](
        fta: StateT[ErrorF, Γ, `Type: x`[A]],
        ftb: StateT[ErrorF, Γ, `Type: x`[B]]
      ): StateT[ErrorF, Γ, `Type.App[_, _]`[T, A, B]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield `Type.App[_, _]`(0, f, a, b)

    extension [T[_[_], _]] (tfunction: StateT[ErrorF, Γ, `Type: lx_xl_x_x`[T]])
      @scala.annotation.targetName("F[G, A]")
      def apply[G[_], A](
        farg: StateT[ErrorF, Γ, `Type: x_x`[G]],
        aarg: StateT[ErrorF, Γ, `Type: x`[A]]
      ): StateT[ErrorF, Γ, `Type.App[_[_], _]`[T, G, A]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
        yield `Type.App[_[_], _]`(0, t, f, a)

    extension [T[_,_,_]] (tfunction: StateT[ErrorF, Γ, `Type.x_x_x_x`[T]])
      @scala.annotation.targetName("F[A, B, C]")
      def apply[A, B, C](
        fta: StateT[ErrorF, Γ, `Type: x`[A]],
        ftb: StateT[ErrorF, Γ, `Type: x`[B]],
        ftc: StateT[ErrorF, Γ, `Type: x`[C]]
      ): StateT[ErrorF, Γ, `Type: x`[T[A, B, C]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
          c <- ftc
        yield `Type.App[_, _, _]`(0, f, a, b, c)

    extension [T[_[_], _, _]] (tfunction: StateT[ErrorF, Γ, `Type.lx_xl_x_x_x`[T]])
      @scala.annotation.targetName("F[G, A, B]")
      def apply[G[_], A, B](
        farg: StateT[ErrorF, Γ, `Type: x_x`[G]],
        aarg: StateT[ErrorF, Γ, `Type: x`[A]],
        barg: StateT[ErrorF, Γ, `Type: x`[B]]
      ): StateT[ErrorF, Γ, `Type: x`[T[G, A, B]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
          b <- barg
        yield `Type.App[_[_], _, _]`(0, t, f, a, b)

    extension [`V.x`[t] <: `Value: x`[t], A, B] (function: StateT[ErrorF, Γ, `Value: x`[A => B]])
      @scala.annotation.targetName("A => B")
      def apply(args: StateT[ErrorF, Γ, `V.x`[A]]): StateT[ErrorF, Γ, `Value: x`[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF(f.tpe.unpure)
        yield `Value.App.1: x`(0, f, a, t)

    extension [A, B] (function: StateT[ErrorF, Γ, `Value: x`[List[A] => B]])
      @scala.annotation.targetName("List[A] => B")
      def apply(vargs: StateT[ErrorF, Γ, `Value: x`[A]]xl: StateT[ErrorF, Γ, `Value.App.Vargs: x`[A, B]] =
        for
          f <- function
          a <- vargs.toList.sequence
          t <- StateT.liftF(f.tpe.unpure)
        yield `Value.App.Vargs: x`(0, f, t, axl


    // extension [
    //   G[_],
    //   `T.x`[t] <: `Type: x`[t],
    //   `V.x`[t] <: `Value: x`[t]
    //   // `V.x`[t] <: `Value: x`[t],
    //   // `V.x_x`[u[_]] <: `Value.x_x`[u]
    // ] (function: StateT[ErrorF, Γ, `Value.x_x`[[A] =>> A => G[A]]])
    //   @scala.annotation.targetName("pure")
    //   def apply[A](arg: StateT[ErrorF, Γ, `V.x`[A]]): StateT[ErrorF, Γ, `Value.App.1: x`[A, G[A], `T.x`, `V.x`]] =
        
    //     for
    //       a <- arg
    //       f <- function
    //       m <- StateT.liftF(f.applyValue(a))
    //     yield m

    extension [G[_]] (function: StateT[ErrorF, Γ, `Value.x_x`[[A] =>> List[A] => G[A]]])
      @scala.annotation.targetName("pureVargs")
      def apply[A](vargs: StateT[ErrorF, Γ, `Value: x`[A]]xl: StateT[ErrorF, Γ, `Value: x`[G[A]]] =
        for
          as <- vargs.toList.sequence
          f <- function
          g <- StateT.liftF(f.applyValue(asxll
        yield g
    
    extension [A, B] (arg1: StateT[ErrorF, Γ, `Value: x`[A]])
      def DOT(func: StateT[ErrorF, Γ, `Value: x`[A => B]])(arg2: StateT[ErrorF, Γ, `Value: x`[B]]): StateT[ErrorF, Γ, `Value: x`[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
        yield `Value.AppDot.1: x`(0, f, a1, a2, a2.tpe)