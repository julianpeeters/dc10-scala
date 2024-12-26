package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.internal.indent.addIndent

trait Objects[F[_]]:
  def OBJECT[T](name: String): F[`Value.*`[T]]
  def OBJECT[T](name: String, contents: F[Unit]): F[`Value.*`[T]]
  def OBJECT[T](name: String, parent: `Type.*`[T], contents: F[Unit]): F[`Value.*`[T]]

object Objects:

  trait Mixins extends Objects[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:
    
    def OBJECT[T](
      name: String
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]](`Type.Var`[T](0, s"$name.type", None))
        v <- StateT.pure(`Value.VarA`(0, name, t))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Statement.`object`[T]](Statement.`object`(v, None, Nil))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield v
      
    def OBJECT[T](
      name: String,
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        t <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[T]](`Type.Var`[T](0, s"$name.type", None))
        v <- StateT.pure(`Value.VarA`(0, name, t))
        d <- StateT.pure(Statement.`object`(v, None, c._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield v

    def OBJECT[T](
      name: String,
      parent: `Type.*`[T],
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        v <- StateT.pure(`Value.VarA`(0, name, parent))
        d <- StateT.pure(Statement.`object`(v, Some(parent), c._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield v
