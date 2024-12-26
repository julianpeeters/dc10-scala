package dc10.scala.predef.calculus

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.{*, given}

trait Variables[F[_]]:
  @scala.annotation.targetName("0")
  def DEF[T](nme: String, tpe: F[`Type.*`[T]]): F[`Value.*`[T]]
  @scala.annotation.targetName("0*")
  def DEF[A, T](nme: String, tparam: F[`Type.Var`[A]], tpe: `Type.*`[A] => F[`Type.*`[T]]): F[`Value.*`[T]]
  @scala.annotation.targetName("0*->*->*")
  def DEF[A, B, T](nme: String, tparam1: F[`Type.Var`[A]], tparam2: F[`Type.Var`[B]], tpe: (`Type.*`[A], `Type.*`[B]) => F[`Type.*`[T]]): F[`Value.*`[T]]
  @scala.annotation.targetName("0*->*")
  def DEF[G[_], T](nme: String, tparam: F[`Type.*->*`[G]], tpe: `Type.*->*`[G] => F[`Type.*`[T]]): F[`Value.*`[T]]
  @scala.annotation.targetName("0(*->*)->*->*")
  def DEF[G[_], A, T](nme: String, tparamf: F[`Type.*->*`[G]], tparama: F[`Type.*`[A]], tpe: (`Type.*->*`[G], `Type.*`[A]) => F[`Type.*`[T]]): F[`Value.*`[T]]
  // 1-arg
  @scala.annotation.targetName("1")
  def DEF[A, T](nme: String, arg: F[`Value.*`[A]], tpe: F[`Type.*`[T]]): F[`Value.*`[A => T]]
  @scala.annotation.targetName("1*")
  def DEF[A, B, T](nme: String, tparam: F[`Type.*`[A]], arg: F[`Value.*`[B]], tpe: `Type.*`[A] => F[`Type.*`[T]]): F[`Value.*`[T]]

  @scala.annotation.targetName("1(*->*)->*")
  def DEF[G[_], A, B, T](nme: String, tparamf: F[`Type.*->*`[G]], tparama: F[`Type.*`[A]], arg: F[`Value.*`[B]], tpe: (`Type.*->*`[G], `Type.*`[A]) => F[`Type.*`[T]]): F[`Value.*`[T]]

  @scala.annotation.targetName("*")
  def TYPE[T](nme: String): F[`Type.Var`[T]]
  @scala.annotation.targetName("*->*")
  def TYPE[G[_], A](nme: String, tparam: F[`Type.Var`[A]]): F[`Type.*->*`[G]]
  @scala.annotation.targetName("(*->*)->*")
  def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type.*->*`[H]]): F[`Type.(*->*)->*`[G]]
  @scala.annotation.targetName("(*->*)->*->*")
  def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type.*->*`[H]], targA: F[`Type.Var`[A]]): F[`Type.(*->*)->*->*`[G]]
  def VAL[T](nme: String, tpe: F[`Type.*`[T]]): F[`Value.*`[T]]

object Variables:

  trait Mixins extends Variables[
    StateT[ErrorF, (Set[LibDep], List[Statement]), _]
  ] with Functions.Mixins:

    @scala.annotation.targetName("0")
    def DEF[T](
      nme: String, 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        ((ds, ms), t) <- StateT.liftF(tpe.runEmpty)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`0`(v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*")
    def DEF[A, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[A]],
      tpe: `Type.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a).runEmpty)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`0`.`[_]`(a, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*->*->*")
    def DEF[A, B, T](
      nme: String,
      tparam1: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[A]],
      tparam2: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[B]],
      tpe: (`Type.*`[A], `Type.*`[B]) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        a <- StateT.liftF(tparam1.runEmptyA)
        b <- StateT.liftF(tparam2.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(a, b).runEmpty)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`0`(v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("0*->*")
    def DEF[G[_], T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
      tpe: `Type.*->*`[G] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        f <- StateT.liftF(tparam.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f).runEmpty)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`0`.`[_[_]]`(f, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v
      
    @scala.annotation.targetName("0(*->*)->*->*")
    def DEF[G[_], A, T](
      nme: String,
      tparamf: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
      tparama: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      tpe: (`Type.*->*`[G], `Type.*`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        f <- StateT.liftF(tparamf.runEmptyA)
        a <- StateT.liftF(tparama.runEmptyA)
        ((ds, ms), t) <- StateT.liftF(tpe(f, a).runEmpty)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`0`.`[_[_], _]`(f, a, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("1")
    def DEF[A, T](
      nme: String,
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]], 
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => T]] =
      for
        a <- StateT.liftF(arg.runEmptyA)
        ((ds, ms), r) <- StateT.liftF(tpe.runEmpty)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]((a.tpe)) ==> tpe
        v <- StateT.pure(`Value.VarA`[A => T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`def`.`1`(a, r, None, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield v

    @scala.annotation.targetName("1*")
    def DEF[A, B, T](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]],
      tpe: `Type.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      ???

    @scala.annotation.targetName("1(*->*)->*")
    def DEF[G[_], A, B, T](
      nme: String,
      tparamf: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]],
      tparama: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]],
      tpe: (`Type.*->*`[G], `Type.*`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      ???
  

    @scala.annotation.targetName("*")
    def TYPE[T](nme: String): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[T]] =
      for
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[T]](`Type.Var`(0, nme, None))
        d <- StateT.pure(Statement.`type`[T](t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("*->*")
    def TYPE[G[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var[_]`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_]`(a, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("(*->*)->*")
    def TYPE[G[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*`[G]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        t <- StateT.pure(`Type.Var[_[_]]`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_]]`(a, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield t

    @scala.annotation.targetName("(*->*)->*->*")
    def TYPE[G[_[_], _], H[_], A](
      nme: String,
      targF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[H]],
      targA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.Var`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*`[G]] =
      for
        f <- StateT.liftF(targF.runEmptyA)
        a <- StateT.liftF(targA.runEmptyA)
        t <- StateT.pure(`Type.Var[_[_], _]`[G](0, nme, None))
        d <- StateT.pure(Statement.`type`.`[_[_], _]`[G, H, A](f, a, t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield t

    def VAL[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        t <- StateT.liftF(tpe.runEmptyA)
        v <- StateT.pure(`Value.VarA`[T](0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`val`(v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield v