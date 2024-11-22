package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.{`object`, TypeExpr}
import dc10.scala.Statement.TypeExpr.`Type`
import dc10.scala.Statement.ValueExpr.`Value`
import dc10.scala.Symbol.Term

trait Objects[F[_]]:
  def OBJECT[T](name: String): F[Value[T]]
  def OBJECT[T, A](name: String, contents: F[A]): F[Value[T]]
  def OBJECT[T, A](name: String, parent: `Type`[T], contents: F[A]): F[Value[T]]

object Objects:

  trait Mixins extends Objects[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:
    
    def OBJECT[T](
      name: String
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        o <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.Var.UserDefinedObject[T]](
          Term.ValueLevel.Var.UserDefinedObject[T](name, Term.TypeLevel.Var.UserDefinedType[T](s"$name.type", None), None, Nil))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `object`[T]](`object`(0, o))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(o)
      
    def OBJECT[T, A](
      name: String,
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.Var.UserDefinedObject[T]](
          Term.ValueLevel.Var.UserDefinedObject[T](name, Term.TypeLevel.Var.UserDefinedType[T](s"$name.type", None), None, c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `object`[T]](`object`(0, o))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(o)

    def OBJECT[T, A](
      name: String,
      parent: `Type`[T],
      contents: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[T]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.Var.UserDefinedObject[T]](
          Term.ValueLevel.Var.UserDefinedObject[T](name, parent.tpe, Some(parent.tpe), c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `object`[T]](`object`(0, o))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(o)
