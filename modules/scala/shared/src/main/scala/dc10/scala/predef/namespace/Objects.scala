package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.{`object`, TypeExpr}
import dc10.scala.Statement.TypeExpr.`Type`
import dc10.scala.Symbol.Object

trait Objects[F[_]]:
  def OBJECT[T, A](name: String, contents: F[A]): F[List[Statement]]
  def OBJECT[T, A](name: String, parent: `Type`[T], contents: F[A]): F[List[Statement]]

object Objects:

  trait Mixins extends Objects[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:
    
    def OBJECT[T, A](
      name: String,
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Object[T]](Object(name, None, c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `object`[T]](`object`(0, o))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield c._2

    def OBJECT[T, A](
      name: String,
      parent: `Type`[T],
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Object[T]](Object(name, Some(parent.tpe), c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `object`[T]](`object`(0, o))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield c._2
