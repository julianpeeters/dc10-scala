package dc10.scala.predef.calculus

import cats.FlatMap
import cats.data.StateT
import cats.implicits.given
import dc10.scala.{*, given}
import dc10.scala.internal.extract.unpure
import dc10.scala.internal.implement.{findImpl, getValue}
import dc10.scala.internal.indent.{addIndent, getIndent}

trait Functions[F[_]]:

  extension [A] (domain: F[`Type.*`[A]])
    @scala.annotation.targetName("fun1T")
    def ==>[B](codomain: F[`Type.*`[B]]): F[`Type.*`[A => B]]

  extension [A, B, C] (domain: F[(`Type.*`[A], `Type.*`[B])])
    @scala.annotation.targetName("fun2T")
    def ==>(codomain: F[`Type.*`[C]]): F[`Type.*`[(A, B) => C]]

  extension [A, B, C, D] (domain: F[(`Type.*`[A], `Type.*`[B], `Type.*`[C])])
    @scala.annotation.targetName("fun3T")
    def ==>(codomain: F[`Type.*`[D]]): F[`Type.*`[(A, B, C) => D]]

  extension [A, B] (fa: F[`Value.*`[A]])
    @scala.annotation.targetName("fun1V")
    def ==>(f: `Value.*`[A] => F[`Value.*`[B]]): F[`Value.*`[A => B]]

  extension [A, B, C] (fa: F[(`Value.*`[A], `Value.*`[B])])
    @scala.annotation.targetName("fun2V")
    def ==>(f: (`Value.*`[A], `Value.*`[B]) => F[`Value.*`[C]]): F[`Value.*`[(A, B) => C]]

  extension [A, B, C, D] (fa: F[(`Value.*`[A], `Value.*`[B], `Value.*`[C])])
    @scala.annotation.targetName("fun3V")
    def ==>(f: (`Value.*`[A], `Value.*`[B], `Value.*`[C]) => F[`Value.*`[D]]): F[`Value.*`[(A, B, C) => D]]

  extension [A] (fa: F[`Type.Var`[A]])
    @scala.annotation.targetName("tLam1")
    def ==>>[G[_]](codomain: `Type.*`[A] => F[`Type.*`[G[A]]]): F[`Type.*->*`[[A]=>> G[A]]]

  def EXT[G[_], B, A](nme: String, tpe: F[`Type.*`[A]])(ext: F[B]): F[B]

  @scala.annotation.targetName("For [_]")
  def FOR[G[_], A](f: F[`Value.*`[A]]): F[`Value.*`[G[A]]]

  @scala.annotation.targetName("For [_[_], _, _]")
  def FOR[T[_[_], _, _], G[_], S, A](f: F[`Value.*`[A]]): F[`Value.*`[T[G, S, A]]]

  extension [G[_]: FlatMap, A] (nme: String)
    def <--(fa: F[`Value.*`[G[A]]]): F[`Value.*`[A]]

object Functions:

  trait Mixins extends Functions[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]
    with Applications.Mixins:
 
    extension [A] (domain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]])
      @scala.annotation.targetName("fun1T")
      def ==>[B](
        codomain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A => B]] =
        for
          a <- domain
          b <- codomain            
        yield `Type.AppInfix[_, _]`(
              0,
              `Type.Var[_, _]`(0, "=>", None),
              a,
              b,
            )

    extension [A, B, C] (domain: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Type.*`[A], `Type.*`[B])])
      @scala.annotation.targetName("fun2T")
      def ==>(
        codomain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[C]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B) => C]] =
        for
          a <- domain
          b <- codomain
          f <- domain ==> codomain
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B) => C]](
            `Type.AppInfix[_, _, _]`(
              0,
              `Type.Var[_, _, _]`(0, "=>", None),
              a._1,
              a._2,
              b,
            )
          )
        yield v

    extension [A, B, C, D] (domain: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Type.*`[A], `Type.*`[B], `Type.*`[C])])
      @scala.annotation.targetName("fun3T")
      def ==>(
        codomain: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[D]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B, C) => D]] =
        for
          a <- domain
          b <- codomain
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B, C) => D]](
            `Type.AppInfix[_, _, _, _]`(
              0,
              `Type.Var[_, _, _, _]`(0, "=>", None),
              a._1,
              a._2,
              a._3,
              b,
            )
          )
        yield v

    extension [A, B] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]])
      @scala.annotation.targetName("fun1V")
      def ==>(
        f: `Value.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]] =
        for
          a <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]](fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure(`Type.AppInfix[_, _]`(
              0,
              `Type.Var[_, _]`[Function1](0, "=>", None),
              a.tpe,
              b.tpe,
            )
          )
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]](`Value.Lam1`(0, a, b, t))
        yield v

    extension [A, B, C] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Value.*`[A], `Value.*`[B])])
      @scala.annotation.targetName("fun2V")
      def ==>(
        f: (`Value.*`[A], `Value.*`[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[C]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[(A, B) => C]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2)
          t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B) => C]](
            `Type.App[_, _, _]`(
              0,
              `Type.Var[_, _, _]`(0, "=>", None),
              a._1.tpe,
              a._2.tpe,
              b.tpe)
          )
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[(A, B) => C]](`Value.Lam2`(0, a._1, a._2, b, t))
        yield v

    extension [A, B, C, D] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Value.*`[A], `Value.*`[B], `Value.*`[C])])
      @scala.annotation.targetName("fun3V")
      def ==>(
        f: (`Value.*`[A], `Value.*`[B], `Value.*`[C]) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[D]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[(A, B, C) => D]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a._1, a._2, a._3)
          t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[(A, B, C) => D]](
            `Type.App[_, _, _, _]`(
              0,
              `Type.Var[_, _, _, _]`(0, "=>", None),
              a._1.tpe,
              a._2.tpe,
              a._3.tpe,
              b.tpe)
          )
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[(A, B, C) => D]](`Value.Lam3`(0, a._1, a._2, a._3, b, t))
        yield v

    extension [A] (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[A]])
      @scala.annotation.targetName("tLam1")
      def ==>>[G[_]](
        codomain: `Type.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[G[A]]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[[A]=>> G[A]]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- codomain(a)
          t <- StateT.pure(`Type.Lam`[[A] =>> G[A], A](0, a, b))
        yield t

    def EXT[G[_], B, A](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]
    )(
      ext: StateT[ErrorF, (Set[LibDep], List[Statement]), B]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), B] =
      for
        ((ds, ms), f) <- StateT.liftF(ext.runEmpty)
        t <- StateT.liftF(tpe.runEmptyA)
        d <- StateT.pure(Statement.`extension`(`Value.VarA`(0, nme, t), ms.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield f

    @scala.annotation.targetName("For [_]")
    def FOR[G[_], A](f: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[G[A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[G[A]])
        v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[G[A]]](`Value.AppForComp`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    @scala.annotation.targetName("For [_[_], _, _]")
    def FOR[T[_[_], _, _], G[_], S, A](f: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T[G, S, A]]] =
      for
        (ctx, a) <- StateT.liftF(f.runEmpty)
        s <- StateT.liftF(ctx.pop(Error("Empty for comprehension")))
        g <- StateT.liftF(s.getValue[T[G, S, A]])
        v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T[G, S, A]]](`Value.AppForComp`(a.getIndent, ctx._2, a, g.tpe))
      yield v

    extension [G[_]: FlatMap, A] (nme: String)
      def <--(
        fa: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[G[A]]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]] =
        for
          g <- fa
          a <- StateT.liftF(g.findImpl.fold(Left(List(Error("Empty generator"))))(i => i.unpure))
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.Var`[A]](`Value.VarC`(g.getIndent, nme, a.tpe, a))
          d <- StateT.pure(Statement.`generator`(`Value.VarC`(g.getIndent, nme, g.tpe, g)))
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v