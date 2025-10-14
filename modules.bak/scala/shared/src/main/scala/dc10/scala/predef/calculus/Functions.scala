package dc10.scala.predef.calculus

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{compiler, *}
// import dc10.scala.internal.construct.ctor
import dc10.scala.internal.extract.unpure
import dc10.scala.internal.implement.{findImpl, getValue}
import dc10.scala.internal.indent.{addIndent, getIndent}
// import dc10.scala.internal.extract.unapply

trait Functions[F[_]]:

  extension [
    A,
    `T1.x`[t] <: `Type: x`[t],
    `T2.x`[t] <: `Type: x`[t]
  ] (domain: F[`T1.x`[A]])
    @scala.annotation.targetName("fun1T")
    def ==>[B](codomain: F[`T2.x`[B]]): F[`Type: x`[A => B]]

  extension [A, B, C] (domain: F[(`Type: x`[A], `Type: x`[B])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[`Type: x`[C]]): F[`Type: x`[(A, B) => C]]

  extension [A, B, C, D] (domain: F[(`Type: x`[A], `Type: x`[B], `Type: x`[C])])
    @scala.annotation.targetName("fun3T")
    def ==>(codomain: F[`Type: x`[D]]): F[`Type: x`[(A, B, C) => D]]

  extension [A, B] (fa: F[`Value.Var.Unbound.Data`[A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: `Value.Var.Unbound.Data`[A] => F[`Value: x`[B]]): F[`Value: x`[A => B]]

  extension [A, B, C] (fa: F[(`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => F[`Value: x`[C]]): F[`Value: x`[(A, B) => C]]

  extension [A, B, C, D] (fa: F[(`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C])])
    @scala.annotation.targetName("fun3V")
    def ==>(f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => F[`Value: x`[D]]): F[`Value: x`[(A, B, C) => D]]

  extension [A] (fa: F[`Type.Var: x`[A]])
    @scala.annotation.targetName("tLam1")
    def ==>>[G[_]](codomain: `Type.Var: x`[A] => F[`Type: x`[G[A]]]): F[`Type.Lam: x_x`[G, A]]

  def EXT[`T.x`[t] <: `Type: x`[t], B, A](nme: String, tpe: F[`T.x`[A]])(ext: F[B]): F[B]

  @scala.annotation.targetName("For [_]")
  def FOR[G[_], A](f: F[`Value: x`[A]]): F[`Value: x`[G[A]]]

  @scala.annotation.targetName("For [_[_], _, _]")
  def FOR[T[_[_], _, _], G[_], S, A](f: F[`Value: x`[A]]): F[`Value: x`[T[G, S, A]]]

  extension [G[_], H[t] <: `Value: x`[t], A] (nme: String)
    def <--(fa: F[H[G[A]]]): F[`Value: x`[A]]

object Functions:

  def function1[A, B](a: `Type: x`[A], b: `Type: x`[B]): `Type: x`[A => B] =
    `Type.AppInfix[_, _]`(0, `Type.Var: x_x_x`(0, "=>", scala.None), a, b)

  def function2[A, B, C](a: `Type: x`[A], b: `Type: x`[B], c: `Type: x`[C]): `Type: x`[(A, B) => C] =
    `Type.AppInfix[_, _, _]`(0, `Type.Var: x_x_x_x`(0, "=>", scala.None), a, b, c)

  def function3[A, B, C, D](a: `Type: x`[A], b: `Type: x`[B], c: `Type: x`[C], d: `Type: x`[D]): `Type: x`[(A, B, C) => D] =
    `Type.AppInfix[_, _, _, _]`(0, `Type.Var: x_x_x_x_x`(0, "=>", scala.None), a, b, c, d)

  def function1[A, B](a: `Value: x`[A], b: `Value: x`[B]): `Value: x`[A => B] =
    `Value.Lam.1: x_x_x x x`(0, a, b, function1(a.tpe, b.tpe))

  // def `function1[_]`[F[_], A](a: `Type.Var: x`[A], fa: `Value: x`[F[A]]): `Value.x_x`[[A] =>> F[A]] =
  //   `Value.Lam.1: x_x`(0, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))

//   def `function1[_]`[F[_], A](a: `Type.Var: x`[A], fa: `Value.Var.Unbound.Data`[F[A]]): `Value.x_x`[[A] =>> F[A]] =
//     // `Value.Lam.1: x_x`(0, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))
//     `Value.App.0: x_x`(0, fa, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))

// // `Value.Lam.1: x_x`[[A] =>> A => Option[A], A](
//         0,
//         function1[A, Option[A]](
//             "x" :: A,
//             `Value.App.1: x`[A, Option[A]](0, `Value.Var.Unbound.Data`(0, "Some", function1(A[A], t)), "x" :: A, t)
//           ),
//         typeLambda1[[A] =>> A => Option[A], A](
//           A[A],
//           function1(A[A], t)
//           // ???//function1(targ, `Type.Var: x_x`[Option](0, "Option", scala.None, scala.List()).applyType(targ))
//         )
//       )


  def function2[A, B, C](a: `Value: x`[A], b: `Value: x`[B], c: `Value: x`[C]): `Value: x`[(A, B) => C] =
    `Value.Lam.2: x_x_x_x x * x`(0, a, b, c, function2(a.tpe, b.tpe, c.tpe))

  def function3[A, B, C, D](a: `Value: x`[A], b: `Value: x`[B], c: `Value: x`[C], d: `Value: x`[D]): `Value: x`[(A, B, C) => D] =
    `Value.Lam.3: x_x_x_x_x x * x x`(0, a, b, c, d, function3(a.tpe, b.tpe, c.tpe, d.tpe))

  def typeLambda1[F[_], A](a: `Type.Var: x`[A], b: `Type: x`[F[A]]): `Type.Lam: x_x`[F, A] =
    `Type.Lam: x_x`[F, A](0, a, b)

  // def typeLambda1[F[_], A, B](a: `Type.Var: x`[A], b: `Type.AppInfix[_, _]`[Function1, A, B]): `Type.Lam: x_x`[F, A] =
  //   `Type.Lam: x_x`[F, A](0, a, b)

  trait Mixins extends Functions[StateT[ErrorF, Γ, _]]
    with Applications.Mixins:
 
    extension [A, `T1.x`[t] <: `Type: x`[t], `T2.x`[u] <: `Type: x`[u]] (domain: StateT[ErrorF, Γ, `T1.x`[A]])
      @scala.annotation.targetName("fun1T")
      def ==>[B](
        codomain: StateT[ErrorF, Γ, `T2.x`[B]]
      ): StateT[ErrorF, Γ, `Type: x`[A => B]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function1(a, b)

    extension [A, B, C] (domain: StateT[ErrorF, Γ, (`Type: x`[A], `Type: x`[B])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, Γ, `Type: x`[C]]
      ): StateT[ErrorF, Γ, `Type: x`[(A, B) => C]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function2(a._1, a._2, b)

    extension [A, B, C, D] (domain: StateT[ErrorF, Γ, (`Type: x`[A], `Type: x`[B], `Type: x`[C])])
      @scala.annotation.targetName("fun3T")
      def ==>(
        codomain: StateT[ErrorF, Γ, `Type: x`[D]]
      ): StateT[ErrorF, Γ, `Type: x`[(A, B, C) => D]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function3(a._1, a._2, a._3, b)

    extension [A, B] (fa: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: `Value.Var.Unbound.Data`[A] => StateT[ErrorF, Γ, `Value: x`[B]]
      ): StateT[ErrorF, Γ, `Value: x`[A => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
        yield function1(a, b)

    extension [A, B, C] (fa: StateT[ErrorF, Γ, (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => StateT[ErrorF, Γ, `Value: x`[C]]
      ): StateT[ErrorF, Γ, `Value: x`[(A, B) => C]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
        yield function2(a._1, a._2, b)

    extension [A, B, C, D] (fa: StateT[ErrorF, Γ, (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C])])
      @scala.annotation.targetName("fun3V")
      def ==>(
        f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => StateT[ErrorF, Γ, `Value: x`[D]]
      ): StateT[ErrorF, Γ, `Value: x`[(A, B, C) => D]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2, a._3)
        yield function3(a._1, a._2, a._3, b)

    extension [A] (fa: StateT[ErrorF, Γ, `Type.Var: x`[A]])
      @scala.annotation.targetName("tLam1")
      def ==>>[G[_]](
        codomain: `Type.Var: x`[A] => StateT[ErrorF, Γ, `Type: x`[G[A]]]
      ): StateT[ErrorF, Γ, `Type.Lam: x_x`[G, A]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- codomain(a)
        yield `Type.Lam: x_x`[[A] =>> G[A], A](0, a, b)

    def EXT[`T.x`[t] <: `Type: x`[t], B, A](
      nme: String,
      tpe: StateT[ErrorF, Γ, `T.x`[A]]
    )(
      ext: StateT[ErrorF, Γ, B]
    ): StateT[ErrorF, Γ, B] =
      for
        ((ds, ms), f) <- StateT.liftF(ext.runEmpty)
        t <- StateT.liftF(tpe.runEmptyA)
        d <- StateT.pure(Statement.`extension`(`Value.Var.Unbound.Data`(0, nme, t), ms.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield f

    @scala.annotation.targetName("For [_]")
    def FOR[G[_], A](f: StateT[ErrorF, Γ, `Value: x`[A]]): StateT[ErrorF, Γ, `Value: x`[G[A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[G[A]])
        v <- StateT.pure[ErrorF, Γ, `Value: x`[G[A]]](`Value.AppForComp: x_x x`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    @scala.annotation.targetName("For [_[_], _, _]")
    def FOR[T[_[_], _, _], G[_], S, A](f: StateT[ErrorF, Γ, `Value: x`[A]]): StateT[ErrorF, Γ, `Value: x`[T[G, S, A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[T[G, S, A]])
        v <- StateT.pure[ErrorF, Γ, `Value: x`[T[G, S, A]]](`Value.AppForComp: x_x x`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    extension [G[_], H[t] <: `Value: x`[t], A] (nme: String)
      def <--(
        fa: StateT[ErrorF, Γ, H[G[A]]]
      ): StateT[ErrorF, Γ, `Value: x`[A]] =
        for
          g <- fa
          a <- StateT.liftF(g.findImpl.fold(Left(List(Error("Empty generator"))))(i => i.unpure))
          v <- StateT.pure[ErrorF, Γ, `Value.Var`[A]](`Value.Var.Bound.Data`(g.getIndent, nme, a.tpe, a))
          d <- StateT.pure(Statement.`generator`(`Value.Var.Bound.Data`(g.getIndent, nme, g.tpe, g)))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v