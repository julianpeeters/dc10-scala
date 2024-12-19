package dc10.scala.predef.binding

import cats.data.StateT
import dc10.scala.{*,  given}
import dc10.scala.dsl.==>

trait Assignments[F[_]]:

  extension [T] (lhs: F[Type.Var[T]])
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

    extension [T] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), Type.Var[T]])
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
          s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
          // _ <- s.narrow.
          // so I get the decl back out,
          // then I see if I can get the val or def out
          // then need to assigne
          // then need reassemble the val or def
          // then ext the new val or def
          (d, v) <- s.implement0(rhs)
          // (d, v) <- s.getValue.map(v => v.implement)
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v

    extension [A, B] (lhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]])
      @scala.annotation.targetName("assign method implementation")
      def :=(rhs: `Value.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          s <- StateT.liftF(ctx.pop(Error("missing method declaration")))
          (d, v) <- s.implement1(rhs)
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v

      @scala.annotation.targetName("assign function value")
      def :=(rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => B]] =
        for
          ctx <- StateT.liftF(lhs.runEmptyS)
          s <- StateT.liftF(ctx.pop(Error("missing function declaration")))
          (d, v) <- s.implement0(rhs)
          _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        yield v

  extension (s: Statement)
    def implement0[T](
      rhs: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Statement, `Value.*`[T])] =
      s match
        case Statement.`case class`(tpe, fields, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`extension`(field, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`(value) => value.findImpl match
          case Some(value) => StateT.liftF(Left(List(Error("Already implemented"))))
          case None =>
            for
              r <- rhs
              v <- StateT.liftF(value.asInstanceOf[`Value.*`[T]].assign(r))
            yield (Statement.`def`.`0`(v), v)
        case Statement.`def`.`0`.`[_]`(tparam, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`1`(arg, ret, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`1`.`[_[_], _]`(tparamf, tparama, arg, impl, ret, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`field`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`generator`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`object`(value, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`package`(nme, contents) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`(tpe, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`(tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_]`(tparam, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_]=>>`(tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_[_]]`(tparam, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`val`(value) =>
          value match
            case Value.VarA(indent, nme, tpe) =>
              for
                r <- rhs
                v <- StateT.liftF(value.asInstanceOf[`Value.*`[T]].assign(r))
              yield (Statement.`val`(v), v)
            case Value.VarC(indent, nme, tpe, impl) => StateT.liftF(Left(List(Error("Already implemented"))))

    def implement1[A, B](
      rhs: `Value.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Statement, `Value.*`[A => B])] =
      s match
        case Statement.`case class`(tpe, fields, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`extension`(field, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`.`[_]`(tparam, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`1`(arg, ret, impl, value) =>
          if !impl.isEmpty then StateT.liftF(Left(List(Error("Not an assignable *"))))
          else
            for
              r <- StateT.liftF(rhs(arg.asInstanceOf[`Value.*`[A]]).runEmptyA)
              f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]](arg.asInstanceOf[`Value.*`[A]]) ==> (a => StateT.pure(r))
              v <- StateT.liftF(value.asInstanceOf[`Value.*`[A => B]].assign(f))
            yield (Statement.`def`.`1`[A, B](arg.asInstanceOf[`Value.*`[A]], r.tpe, Some(r), v), v)
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`def`.`1`.`[_[_], _]`(f, a, arg, impl, ret, value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`field`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`generator`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`object`(value, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`package`(nme, contents) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`(tpe, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`(tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_]`(tparam, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_]=>>`(tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_[_]]`(tparam, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe) => StateT.liftF(Left(List(Error("Not a declaration *"))))
        case Statement.`val`(value) => StateT.liftF(Left(List(Error("Not a declaration *"))))