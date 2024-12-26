package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.internal.implement.assign

trait Assignments[F[_]]:

  extension [T] (lhs: F[`Type.Var`[T]])
    @scala.annotation.targetName("*")
    def :=(rhs: F[`Type.*`[T]]): F[`Type.*`[T]]
    @scala.annotation.targetName("*->*")
    def :=[G[_]](rhs: F[`Type.*->*`[G]]): F[`Type.*->*`[G]]

  extension [T] (lhs: F[`Value.*`[T]])
    @scala.annotation.targetName("assign value")
    def :=(rhs: F[`Value.*`[T]]): F[`Value.*`[T]]

  extension [A, B] (lhs: F[`Value.*`[A => B]])
    @scala.annotation.targetName("assign method implementation")
    def :=(rhs: `Value.*`[A] => F[`Value.*`[B]]): F[`Value.*`[A => B]]
    @scala.annotation.targetName("assign function value")
    def :=(rhs: F[`Value.*`[A => B]]): F[`Value.*`[A => B]]

object Assignments:

  trait Mixins extends Assignments[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[T]])
      @scala.annotation.targetName("*")
      def :=(
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`[T](t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield t

      @scala.annotation.targetName("*->*")
      def :=[G[_]](
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`.`[_]=>>`[G](t))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield t

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]])
      @scala.annotation.targetName("assign value")
      def :=(
        rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          r <- StateT.liftF(rhs.runEmptyA)
          s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
          (d, v) <- StateT.liftF(s.assign(r))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v

    extension [A, B] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]])
      @scala.annotation.targetName("assign method implementation")
      def :=(rhs: `Value.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          s <- StateT.liftF(ctx.pop(Error("missing method declaration")))
          (d, v) <- StateT.liftF(s.assign(rhs.andThen(m => m.runEmptyA)))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v

      @scala.annotation.targetName("assign function value")
      def :=(rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          r <- StateT.liftF(rhs.runEmptyA)
          s <- StateT.liftF(ctx.pop(Error("missing function declaration")))
          (d, v) <- StateT.liftF(s.assign(r))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v
