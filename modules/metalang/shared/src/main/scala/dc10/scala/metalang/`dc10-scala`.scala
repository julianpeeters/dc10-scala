package dc10.scala.metalang

import cats.data.StateT
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.TypeExpr.`Type`
import dc10.scala.Symbol.Term

trait `dc10-scala`[F[_]]:
  def TYPEEXPR[G[_], A](targ: F[`Type`[A]]): F[`Type`[G[A]]]

object `dc10-scala`:

  val lib: LibDep = LibDep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends `dc10-scala`[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    def TYPEEXPR[G[_], A](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield Type(
        Term.TypeLevel.App.`App[_]`(
          Term.TypeLevel.Var.`UserDefinedType[_]`("dc10.scala.Statement.TypeExpr.`Type`", None),
          a.tpe
          )
        )
      