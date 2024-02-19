package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{ErrorF, Statement, Symbol}
import dc10.scala.Statement.{ObjectDef, TypeExpr}
import dc10.scala.ctx.ext

trait Objects[F[_]]:
  def OBJECT[T, A](name: String, contents: F[A]): F[List[Statement]]
  def OBJECT[T, A](name: String, parent: TypeExpr[T, Unit], contents: F[A]): F[List[Statement]]

object Objects:

  trait Mixins extends Objects[[A] =>> StateT[ErrorF, List[Statement], A]]:
    
    def OBJECT[T, A](name: String, contents: StateT[ErrorF, List[Statement], A]): StateT[ErrorF, List[Statement], List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, List[Statement], List[Statement]](contents.runEmptyS)
        o <- StateT.pure[ErrorF, List[Statement], Symbol.Object[T, Unit]](Symbol.Object(None, name, None, c.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, List[Statement], ObjectDef](ObjectDef(o, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield c

    def OBJECT[T, A](name: String, parent: TypeExpr[T, Unit], contents: StateT[ErrorF, List[Statement], A]): StateT[ErrorF, List[Statement], List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, List[Statement], List[Statement]](contents.runEmptyS)
        o <- StateT.pure[ErrorF, List[Statement], Symbol.Object[T, Unit]](Symbol.Object(None, name, Some(parent.tpe), c.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, List[Statement], ObjectDef](ObjectDef(o, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield c
