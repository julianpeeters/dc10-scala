package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.internal.implement.assign

trait Assignments[F[_]]:

  extension [T] (lhs: F[`Type.Var: x`[T]])
    @scala.annotation.targetName("Var*")
    def :=(rhs: F[`Type.Var: x`[T]]): F[`Type.Var: x`[T]]
    @scala.annotation.targetName("App*")
    def :=[G[_], A](rhs: F[`Type.App[_]`[G, A]]): F[`Type.Var: x`[G[A]]]
    @scala.annotation.targetName("App(x→x)→x→x")
    def :=[G[_[_], _], H[_], A](rhs: F[`Type.App[_[_], _]`[G, H, A]]): F[`Type.Var: x`[G[H, A]]]
    @scala.annotation.targetName("Lamx→x")
    def :=[G[_], A](rhs: F[`Type.Lam: x→x`[G, A]]): F[`Type.Var: x→x`[G]]
 
  extension [`V.x`[t] <: `Value: x`[t],  T] (lhs: F[`Value.Var.Unbound.Data`[T]])
    @scala.annotation.targetName("assign value")
    def :=(rhs: F[`V.x`[T]]): F[`Value: x`[T]]

  extension [A, B] (lhs: F[`Value.Var.Unbound.Data`[A => B]])
    @scala.annotation.targetName("assign method implementation")
    def :=(rhs: `Value: x`[A] => F[`Value: x`[B]]): F[`Value: x`[A => B]]
    @scala.annotation.targetName("assign function value")
    def :=(rhs: F[`Value: x`[A => B]]): F[`Value: x`[A => B]]

object Assignments:

  extension (s: String)
    // def :=[A](rhs: `Value: x`[A]): `Value.Var.Bound.Data`[A] =
    //   `Value.Var.Bound.Data`[A](0, s, rhs.tpe, rhs)
    def :=[F[_], G[_]](rhs: `Value.x→x`[[A] =>> F[A] => G[A]]): `Value.Var1[_]`[F, G] =
      `Value.Var1[_]`(0, s, rhs.tpe, Some(rhs))

  trait Mixins extends Assignments[StateT[ErrorF, Γ, _]]:

    extension [T] (lhs: StateT[ErrorF, Γ, `Type.Var: x`[T]])
      @scala.annotation.targetName("Var*")
      def :=(
        rhs: StateT[ErrorF, Γ, `Type.Var: x`[T]]
      ): StateT[ErrorF, Γ, `Type.Var: x`[T]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`[T](t))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield t

      @scala.annotation.targetName("App*")
      def :=[G[_], A](
        rhs: StateT[ErrorF, Γ, `Type.App[_]`[G, A]]
      ): StateT[ErrorF, Γ, `Type.Var: x`[G[A]]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`[G[A]](t))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield t

      @scala.annotation.targetName("App(x→x)→x→x")
      def :=[G[_[_], _], H[_], A](
        rhs: StateT[ErrorF, Γ, `Type.App[_[_], _]`[G, H, A]]
      ): StateT[ErrorF, Γ, `Type.Var: x`[G[H, A]]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`[G[H, A]](t))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield t

      @scala.annotation.targetName("Lamx→x")
      def :=[G[_], A](
        rhs: StateT[ErrorF, Γ, `Type.Lam: x→x`[G, A]]
      ): StateT[ErrorF, Γ, `Type.Var: x→x`[G]] =
        for
          l <- StateT.liftF(lhs.runEmptyA)
          r <- StateT.liftF(rhs.runEmptyA)
          t <- StateT.liftF(l.assign(r))
          d <- StateT.pure(Statement.`type`.`[_]=>>`[G](t))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield t


    extension [`V.x`[t] <: `Value: x`[t], T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]])
      @scala.annotation.targetName("assign value")
      def :=(
        rhs: StateT[ErrorF, Γ, `V.x`[T]]
      ): StateT[ErrorF, Γ, `Value: x`[T]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          r <- StateT.liftF(rhs.runEmptyA)
          s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
          (d, v) <- StateT.liftF(s.assign(r))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A => B]])
      @scala.annotation.targetName("assign method implementation")
      def :=(rhs: `Value: x`[A] => StateT[ErrorF, Γ, `Value: x`[B]]): StateT[ErrorF, Γ, `Value: x`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          s <- StateT.liftF(ctx.pop(Error("missing method declaration")))
          (d, v) <- StateT.liftF(s.assign(rhs.andThen(m => m.runEmptyA)))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      @scala.annotation.targetName("assign function value")
      def :=(rhs: StateT[ErrorF, Γ, `Value: x`[A => B]]): StateT[ErrorF, Γ, `Value: x`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          r <- StateT.liftF(rhs.runEmptyA)
          s <- StateT.liftF(ctx.pop(Error("missing function declaration")))
          (d, v) <- StateT.liftF(s.assign(r))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
