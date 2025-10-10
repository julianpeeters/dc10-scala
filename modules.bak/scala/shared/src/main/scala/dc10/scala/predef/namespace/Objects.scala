package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.internal.indent.addIndent

trait Objects[F[_]]:
  def OBJECT[T](name: String): F[`Value: x`[T]]
  def OBJECT[T](name: String, contents: F[Unit]): F[`Value: x`[T]]
  def OBJECT[T](name: String, parent: `Type: x`[T], contents: F[Unit]): F[`Value: x`[T]]

object Objects:

  trait Mixins extends Objects[StateT[ErrorF, Γ, _]]:
    
    def OBJECT[T](
      name: String
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var: x`[T](0, s"$name.type", None))
        v <- StateT.pure(`Value.Var.Unbound.Data`(0, name, t))
        d <- StateT.pure[ErrorF, Γ, Statement.`object`[T]](Statement.`object`(v, None, Nil))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v
      
    def OBJECT[T](
      name: String,
      contents: StateT[ErrorF, Γ, Unit]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
        t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var: x`[T](0, s"$name.type", None))
        v <- StateT.pure(`Value.Var.Unbound.Data`(0, name, t))
        d <- StateT.pure(Statement.`object`(v, None, c._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v

    def OBJECT[T](
      name: String,
      parent: `Type: x`[T],
      contents: StateT[ErrorF, Γ, Unit]
    ): StateT[ErrorF, Γ, `Value: x`[T]] =
      for
        c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
        v <- StateT.pure(`Value.Var.Unbound.Data`(0, name, parent))
        d <- StateT.pure(Statement.`object`(v, Some(parent), c._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield v
