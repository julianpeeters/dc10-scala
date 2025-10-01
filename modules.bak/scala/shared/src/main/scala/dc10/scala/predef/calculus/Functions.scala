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
    `T1.*`[t] <: `Type.Expr: *`[t],
    `T2.*`[t] <: `Type.Expr: *`[t]
  ] (domain: F[`T1.*`[A]])
    @scala.annotation.targetName("fun1T")
    def ==>[B](codomain: F[`T2.*`[B]]): F[`Type.Expr: *`[A => B]]

  extension [A, B, C] (domain: F[(`Type.Expr: *`[A], `Type.Expr: *`[B])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[`Type.Expr: *`[C]]): F[`Type.Expr: *`[(A, B) => C]]

  extension [A, B, C, D] (domain: F[(`Type.Expr: *`[A], `Type.Expr: *`[B], `Type.Expr: *`[C])])
    @scala.annotation.targetName("fun3T")
    def ==>(codomain: F[`Type.Expr: *`[D]]): F[`Type.Expr: *`[(A, B, C) => D]]

  extension [A, B] (fa: F[`Value.Var.Unbound.Data`[A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: `Value.Var.Unbound.Data`[A] => F[`Value.Expr: *`[B]]): F[`Value.Expr: *`[A => B]]

  extension [A, B, C] (fa: F[(`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => F[`Value.Expr: *`[C]]): F[`Value.Expr: *`[(A, B) => C]]

  extension [A, B, C, D] (fa: F[(`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C])])
    @scala.annotation.targetName("fun3V")
    def ==>(f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => F[`Value.Expr: *`[D]]): F[`Value.Expr: *`[(A, B, C) => D]]

  extension [A] (fa: F[`Type.Var: *`[A]])
    @scala.annotation.targetName("tLam1")
    def ==>>[G[_]](codomain: `Type.Var: *`[A] => F[`Type.Expr: *`[G[A]]]): F[`Type.Lam: *→*`[G, A]]

  def EXT[`T.*`[t] <: `Type.Expr: *`[t], B, A](nme: String, tpe: F[`T.*`[A]])(ext: F[B]): F[B]

  @scala.annotation.targetName("For [_]")
  def FOR[G[_], A](f: F[`Value.Expr: *`[A]]): F[`Value.Expr: *`[G[A]]]

  @scala.annotation.targetName("For [_[_], _, _]")
  def FOR[T[_[_], _, _], G[_], S, A](f: F[`Value.Expr: *`[A]]): F[`Value.Expr: *`[T[G, S, A]]]

  extension [G[_], H[t] <: `Value.Expr: *`[t], A] (nme: String)
    def <--(fa: F[H[G[A]]]): F[`Value.Expr: *`[A]]

object Functions:

  def function1[A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *`[B]): `Type.Expr: *`[A => B] =
    `Type.AppInfix[_, _]`(0, `Type.Var: *→*→*`(0, "=>", scala.None), a, b)

  def function2[A, B, C](a: `Type.Expr: *`[A], b: `Type.Expr: *`[B], c: `Type.Expr: *`[C]): `Type.Expr: *`[(A, B) => C] =
    `Type.AppInfix[_, _, _]`(0, `Type.Var: *→*→*→*`(0, "=>", scala.None), a, b, c)

  def function3[A, B, C, D](a: `Type.Expr: *`[A], b: `Type.Expr: *`[B], c: `Type.Expr: *`[C], d: `Type.Expr: *`[D]): `Type.Expr: *`[(A, B, C) => D] =
    `Type.AppInfix[_, _, _, _]`(0, `Type.Var: *→*→*→*→*`(0, "=>", scala.None), a, b, c, d)

  def function1[A, B](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B]): `Value.Expr: *`[A => B] =
    `Value.Lam.1: *→*→* * *`(0, a, b, function1(a.tpe, b.tpe))

  // def `function1[_]`[F[_], A](a: `Type.Var: *`[A], fa: `Value.Expr: *`[F[A]]): `Value.*→*`[[A] =>> F[A]] =
  //   `Value.Lam.1: *→*`(0, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))

//   def `function1[_]`[F[_], A](a: `Type.Var: *`[A], fa: `Value.Var.Unbound.Data`[F[A]]): `Value.*→*`[[A] =>> F[A]] =
//     // `Value.Lam.1: *→*`(0, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))
//     `Value.App.0: *→*`(0, fa, fa, typeLambda1[[A] =>> F[A], A](a, fa.tpe))

// // `Value.Lam.1: *→*`[[A] =>> A => Option[A], A](
//         0,
//         function1[A, Option[A]](
//             "x" :: A,
//             `Value.App.1: *`[A, Option[A]](0, `Value.Var.Unbound.Data`(0, "Some", function1(A[A], t)), "x" :: A, t)
//           ),
//         typeLambda1[[A] =>> A => Option[A], A](
//           A[A],
//           function1(A[A], t)
//           // ???//function1(targ, `Type.Var: *→*`[Option](0, "Option", scala.None, scala.List()).applyType(targ))
//         )
//       )


  def function2[A, B, C](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B], c: `Value.Expr: *`[C]): `Value.Expr: *`[(A, B) => C] =
    `Value.Lam.2: *→*→*→* * * *`(0, a, b, c, function2(a.tpe, b.tpe, c.tpe))

  def function3[A, B, C, D](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B], c: `Value.Expr: *`[C], d: `Value.Expr: *`[D]): `Value.Expr: *`[(A, B, C) => D] =
    `Value.Lam.3: *→*→*→*→* * * * *`(0, a, b, c, d, function3(a.tpe, b.tpe, c.tpe, d.tpe))

  def typeLambda1[F[_], A](a: `Type.Var: *`[A], b: `Type.Expr: *`[F[A]]): `Type.Lam: *→*`[F, A] =
    `Type.Lam: *→*`[F, A](0, a, b)

  // def typeLambda1[F[_], A, B](a: `Type.Var: *`[A], b: `Type.AppInfix[_, _]`[Function1, A, B]): `Type.Lam: *→*`[F, A] =
  //   `Type.Lam: *→*`[F, A](0, a, b)

  trait Mixins extends Functions[StateT[ErrorF, Γ, _]]
    with Applications.Mixins:
 
    extension [A, `T1.*`[t] <: `Type.Expr: *`[t], `T2.*`[u] <: `Type.Expr: *`[u]] (domain: StateT[ErrorF, Γ, `T1.*`[A]])
      @scala.annotation.targetName("fun1T")
      def ==>[B](
        codomain: StateT[ErrorF, Γ, `T2.*`[B]]
      ): StateT[ErrorF, Γ, `Type.Expr: *`[A => B]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function1(a, b)

    extension [A, B, C] (domain: StateT[ErrorF, Γ, (`Type.Expr: *`[A], `Type.Expr: *`[B])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, Γ, `Type.Expr: *`[C]]
      ): StateT[ErrorF, Γ, `Type.Expr: *`[(A, B) => C]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function2(a._1, a._2, b)

    extension [A, B, C, D] (domain: StateT[ErrorF, Γ, (`Type.Expr: *`[A], `Type.Expr: *`[B], `Type.Expr: *`[C])])
      @scala.annotation.targetName("fun3T")
      def ==>(
        codomain: StateT[ErrorF, Γ, `Type.Expr: *`[D]]
      ): StateT[ErrorF, Γ, `Type.Expr: *`[(A, B, C) => D]] =
        for
          a <- StateT.liftF(domain.runEmptyA)
          b <- StateT.liftF(codomain.runEmptyA)
        yield function3(a._1, a._2, a._3, b)

    extension [A, B] (fa: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: `Value.Var.Unbound.Data`[A] => StateT[ErrorF, Γ, `Value.Expr: *`[B]]
      ): StateT[ErrorF, Γ, `Value.Expr: *`[A => B]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
        yield function1(a, b)

    extension [A, B, C] (fa: StateT[ErrorF, Γ, (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => StateT[ErrorF, Γ, `Value.Expr: *`[C]]
      ): StateT[ErrorF, Γ, `Value.Expr: *`[(A, B) => C]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
        yield function2(a._1, a._2, b)

    extension [A, B, C, D] (fa: StateT[ErrorF, Γ, (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C])])
      @scala.annotation.targetName("fun3V")
      def ==>(
        f: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => StateT[ErrorF, Γ, `Value.Expr: *`[D]]
      ): StateT[ErrorF, Γ, `Value.Expr: *`[(A, B, C) => D]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2, a._3)
        yield function3(a._1, a._2, a._3, b)

    extension [A] (fa: StateT[ErrorF, Γ, `Type.Var: *`[A]])
      @scala.annotation.targetName("tLam1")
      def ==>>[G[_]](
        codomain: `Type.Var: *`[A] => StateT[ErrorF, Γ, `Type.Expr: *`[G[A]]]
      ): StateT[ErrorF, Γ, `Type.Lam: *→*`[G, A]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- codomain(a)
        yield `Type.Lam: *→*`[[A] =>> G[A], A](0, a, b)

    def EXT[`T.*`[t] <: `Type.Expr: *`[t], B, A](
      nme: String,
      tpe: StateT[ErrorF, Γ, `T.*`[A]]
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
    def FOR[G[_], A](f: StateT[ErrorF, Γ, `Value.Expr: *`[A]]): StateT[ErrorF, Γ, `Value.Expr: *`[G[A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[G[A]])
        v <- StateT.pure[ErrorF, Γ, `Value.Expr: *`[G[A]]](`Value.AppForComp: *→* *`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    @scala.annotation.targetName("For [_[_], _, _]")
    def FOR[T[_[_], _, _], G[_], S, A](f: StateT[ErrorF, Γ, `Value.Expr: *`[A]]): StateT[ErrorF, Γ, `Value.Expr: *`[T[G, S, A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[T[G, S, A]])
        v <- StateT.pure[ErrorF, Γ, `Value.Expr: *`[T[G, S, A]]](`Value.AppForComp: *→* *`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    extension [G[_], H[t] <: `Value.Expr: *`[t], A] (nme: String)
      def <--(
        fa: StateT[ErrorF, Γ, H[G[A]]]
      ): StateT[ErrorF, Γ, `Value.Expr: *`[A]] =
        for
          g <- fa
          a <- StateT.liftF(g.findImpl.fold(Left(List(Error("Empty generator"))))(i => i.unpure))
          v <- StateT.pure[ErrorF, Γ, `Value.Var`[A]](`Value.Var.Bound.Data`(g.getIndent, nme, a.tpe, a))
          d <- StateT.pure(Statement.`generator`(`Value.Var.Bound.Data`(g.getIndent, nme, g.tpe, g)))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v