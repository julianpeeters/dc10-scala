package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{*,  given}

trait TemplateTypes[F[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[T, A](name: String, fields: F[`Value.*`[A]]): F[(`Type.*`[T], `Value.*`[A => T])]
  @scala.annotation.targetName("caseClass2")
  def CASECLASS[T, A, B](name: String, fields: F[(`Value.*`[A], `Value.*`[B])]): F[(`Type.*`[T], `Value.*`[(A, B) => T])]
  def FIELD[T](nme: String, tpe: F[`Type.*`[T]]): F[`Value.*`[T]]
  @scala.annotation.targetName("trait*")
  def TRAIT[T](nme: String, members: F[Unit]): F[`Type.*`[T]]
  @scala.annotation.targetName("trait*extends")
  def TRAIT[T, A](nme: String, parent: F[`Type.*`[A]], members: F[Unit]): F[`Type.*`[T]]
  @scala.annotation.targetName("trait*->*")
  def TRAIT[T[_], A](nme: String, tparam: F[`Type.*`[A]], members: `Type.*`[A] => F[Unit]): F[`Type.*->*`[T]]
  @scala.annotation.targetName("trait(*->*)->*")
  def TRAIT[T[_[_]], H[_]](nme: String, tparam: F[`Type.*->*`[H]], members: `Type.*->*`[H] => F[Unit]): F[`Type.(*->*)->*`[T]]
  @scala.annotation.targetName("trait(*->*)->*->*")
  def TRAIT[T[_[_], _], H[_], A](nme: String, tparamF: F[`Type.*->*`[H]], tparamA: F[`Type.*`[A]], members: (`Type.*->*`[H], `Type.*`[A]) => F[Unit]): F[`Type.(*->*)->*->*`[T]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[T, A](
      name: String,
      fields: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (`Type.*`[T], `Value.*`[A => T])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), ((Set[LibDep], List[Statement]), `Value.*`[A])](fields.runEmpty)
        n <- StateT.pure(Type.`Var`[T](0, name, None))
        v <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A => T]](
          a match
            case Value.VarA(i, nme, tpe) => Right[List[Error], `Value.*`[A => T]](
              Value.VarA(
                indent = 0,
                nme = name,
                tpe = Type.`App[_, _]`(0, Type.`Var[_, _]`(0, "=>", None), a.tpe, n),
              )
            )
            case _ => Left(List(Error(s"Expected Identifier but found ${a}")))
          )
        d <- StateT.pure(Statement.`case class`(n, fields._2.map(s => s.addIndent), Nil))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (n, v)

    @scala.annotation.targetName("caseClass2")
    def CASECLASS[T, A, B](
      name: String,
      fields: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Value.*`[A], `Value.*`[B])]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (`Type.*`[T], `Value.*`[(A, B) => T])] =
      for
        (fields, (a, b)) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), ((Set[LibDep], List[Statement]), (`Value.*`[A], `Value.*`[B]))](fields.runEmpty)
        n <- StateT.pure(Type.`Var`[T](0, name, None))
        v <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[(A, B) => T]](
          (a, b) match
            case (Value.VarA(indent, nme, tpe), Value.VarA(indent2, nme2, tpe2)) =>
              Right[List[Error], `Value.*`[(A, B) => T]](
                Value.VarA(
                  indent = 0,
                  nme = name,
                  tpe = Type.`App[_, _, _]`(
                    0,
                    Type.`Var[_, _, _]`(0, "=>", None),
                    a.tpe,
                    b.tpe,
                    n
                  )
                )
              )
            case _ => Left(List(Error(s"Expected Identifier but found ${a}")))
          )
        d <- StateT.pure(Statement.`case class`(n, fields._2.map(s => s.addIndent), Nil))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (n, v)

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        t <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]](tpe.runEmptyA)
        v <- StateT.pure(Value.VarA(0, nme, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement](Statement.`field`(v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield v 
  
    @scala.annotation.targetName("trait*")
    def TRAIT[T](
      nme: String,
      members: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]] =
      for
        b <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members.runEmptyS)
        t <- StateT.pure(Type.Var(0, nme, None))
        d <- StateT.pure(Statement.`trait`(t, None, b._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Type.`Var`(0, nme, None)

    @scala.annotation.targetName("trait*extends")
    def TRAIT[T, A](
      nme: String,
      parent: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      members: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]] =
      for
        p <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]](parent.runEmptyA)
        b <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members.runEmptyS)
        t <- StateT.pure(Type.Var(0, nme, None))
        d <- StateT.pure(Statement.`trait`(t, Some(p), b._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Type.`Var`(0, nme, None)

    @scala.annotation.targetName("trait*->*")
    def TRAIT[T[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      members: `Type.*`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        t <- StateT.pure(Type.`Var[_]`(0, nme, None))
        d <- StateT.pure(Statement.`trait`.`[_]`(t, a, None, ms.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Type.`Var[_]`(0, nme, None)

    @scala.annotation.targetName("trait(*->*)->*")
    def TRAIT[T[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[H]],
      members: `Type.*->*`[H] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        t <- StateT.pure(Type.`Var[_[_]]`(0, nme, None))
        d <- StateT.pure(Statement.`trait`.`[_[_]]`(t, a, None, ms.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Type.`Var[_[_]]`(0, nme, None)

    @scala.annotation.targetName("trait(*->*)->*->*")
    def TRAIT[T[_[_], _], H[_], A](
      nme: String,
      tparamF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[H]],
      tparamA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]],
      members: (`Type.*->*`[H], `Type.*`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*`[T]] =
      for
        f <- StateT.liftF(tparamF.runEmptyA)
        a <- StateT.liftF(tparamA.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(f, a).runEmptyS)
        t <- StateT.pure(Type.`Var[_[_], _]`(0, nme, None))
        d <- StateT.pure(Statement.`trait`.`[_[_], _]`(t, f, a, None, ms.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield Type.`Var[_[_], _]`(0, nme, None)