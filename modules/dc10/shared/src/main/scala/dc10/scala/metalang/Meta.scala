package dc10.scala.dc10

import cats.data.StateT
import dc10.scala.{ErrorF, Statement, Symbol}
import dc10.scala.ctx.dep
import dc10.scala.Statement.{LibraryDependency, TypeExpr}

trait Meta[F[_]]:
  def TYPEEXPR[G[_], T](targ: F[TypeExpr[T]]): F[TypeExpr[G[T]]]

object Meta:

  val lib: LibraryDependency = LibraryDependency(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends Meta[[A] =>> StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]]:

    def TYPEEXPR[G[_], T](
      targ: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[T]]
    ): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), TypeExpr[G[T]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.dep(Meta.lib))
      yield TypeExpr(Symbol.Term.TypeLevel.App.App1(Symbol.Term.TypeLevel.Var.UserDefinedType("dc10.scala.Statement.TypeExpr", Nil, None), a.tpe))
      