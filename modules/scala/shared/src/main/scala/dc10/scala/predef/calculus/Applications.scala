package dc10.scala.predef.calculus

import cats.data.StateT
import cats.implicits.given
import dc10.scala.*

trait Applications[F[_]]:

  extension [T[_]] (function: F[`Type.*->*`[T]])
    @scala.annotation.targetName("F[A]")
    def apply[A](args: F[`Type.*`[A]]): F[`Type.*`[T[A]]]

  extension [T[_[_]]] (tfunction: F[`Type.(*->*)->*`[T]])
    @scala.annotation.targetName("F[G]")
    def apply[G[_]](farg: F[`Type.*->*`[G]]): F[`Type.*`[T[G]]]

  extension [T[_,_]] (tfunction: F[`Type.*->*->*`[T]])
    @scala.annotation.targetName("F[A, B]")
    def apply[A, B](fta: F[`Type.*`[A]], ftb: F[`Type.*`[B]]): F[`Type.*`[T[A, B]]]

  extension [T[_[_],_]] (tfunction: F[`Type.(*->*)->*->*`[T]])
    @scala.annotation.targetName("F[G, A]")
    def apply[G[_], A](farg: F[`Type.*->*`[G]], aarg: F[`Type.*`[A]]): F[`Type.*`[T[G, A]]]

  extension [T[_[_],_,_]] (tfunction: F[`Type.(*->*)->*->*->*`[T]])
    @scala.annotation.targetName("F[G, A, B]")
    def apply[G[_], A, B](farg: F[`Type.*->*`[G]], aarg: F[`Type.*`[A]], barg: F[`Type.*`[B]]): F[`Type.*`[T[G, A, B]]]

  extension [A, B] (function: F[`Value.*`[A => B]])
    @scala.annotation.targetName("A => B")
    def apply(args: F[`Value.*`[A]]): F[`Value.*`[B]]

  extension [A, B] (function: F[`Value.*`[List[A] => B]])
    @scala.annotation.targetName("List[A] => B")
    def apply(vargs: F[`Value.*`[A]]*): F[`Value.*`[B]]

  extension [G[_]] (function: F[`Value.*->*`[[A] =>> A => G[A]]])
    @scala.annotation.targetName("pure")
    def apply[A](arg: F[`Value.*`[A]]): F[`Value.*`[G[A]]]

  extension [G[_]] (function: F[`Value.*->*`[[A] =>> List[A] => G[A]]])
    @scala.annotation.targetName("pureVargs")
    def apply[A](vargs: F[`Value.*`[A]]*): F[`Value.*`[G[A]]]

  extension [A, B] (arg1: F[`Value.*`[A]])
    def DOT(func: F[`Value.*`[A => B]])(arg2: F[`Value.*`[B]]): F[`Value.*`[B]]

object Applications:

  trait Mixins extends Applications[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    extension [T[_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[T]])
      @scala.annotation.targetName("F[A]")
      def apply[A](
        args: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[A]]] =
        for
          f <- tfunction
          a <- args
        yield Type.`App[_]`(0, f, a)

    extension [T[_[_]]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*`[T]])
      @scala.annotation.targetName("F[G]")
      def apply[G[_]](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[G]]] =
        for
          t <- tfunction
          f <- farg
        yield Type.`App[_[_]]`(0, t, f)

    extension [T[_,_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*->*`[T]])
      @scala.annotation.targetName("F[A, B]")
      def apply[A, B](
        fta: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
        ftb: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[A, B]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield Type.`App[_, _]`(0, f, a, b)

    extension [T[_[_], _]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*`[T]])
      @scala.annotation.targetName("F[G, A]")
      def apply[G[_], A](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
        aarg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[G, A]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
        yield Type.`App[_[_], _]`(0, t, f, a)

    extension [T[_,_,_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*->*->*`[T]])
      @scala.annotation.targetName("F[A, B, C]")
      def apply[A, B, C](
        fta: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
        ftb: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[B]],
        ftc: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[C]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[A, B, C]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
          c <- ftc
        yield Type.`App[_, _, _]`(0, f, a, b, c)

    extension [T[_[_], _, _]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*->*`[T]])
      @scala.annotation.targetName("F[G, A, B]")
      def apply[G[_], A, B](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
        aarg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
        barg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T[G, A, B]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
          b <- barg
        yield Type.`App[_[_], _, _]`(0, t, f, a, b)

    extension [A, B] (function: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]])
      @scala.annotation.targetName("A => B")
      def apply(args: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF(f.tpe.unapplyRightmost)
        yield Value.App1(0, f, a, t)

    extension [A, B] (function: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[List[A] => B]])
      @scala.annotation.targetName("List[A] => B")
      def apply(vargs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]*): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]] =
        for
          f <- function
          a <- vargs.toList.sequence
          t <- StateT.liftF(f.tpe.unapplyRightmost)
        yield Value.AppVargs(0, f, t, a*)


    extension [G[_]] (function: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> A => G[A]]])
      @scala.annotation.targetName("pure")
      def apply[A](arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[G[A]]] =
        
        for
          a <- arg
          f <- function
          m <- StateT.liftF(f.apply(a))
        yield m

    extension [G[_]] (function: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> List[A] => G[A]]])
      @scala.annotation.targetName("pureVargs")
      def apply[A](vargs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]*): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[G[A]]] =
        for
          as <- vargs.toList.sequence
          f <- function
          g <- StateT.liftF(f.apply(as*))
        yield g
    
    extension [A, B] (arg1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]])
      def DOT(func: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]])(arg2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]](Value.AppDot1(0, f, a1, a2, a2.tpe))
        yield v