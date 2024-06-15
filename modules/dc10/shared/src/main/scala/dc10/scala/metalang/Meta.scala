package dc10.scala.dc10

import cats.data.StateT
import dc10.scala.ctx.dep
import dc10.scala.{ErrorF, LibDep, Statement}
import dc10.scala.Statement.TypeExpr.`Type`
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos

trait Meta[F[_]]:
  def TYPEEXPR[G[_], T](targ: F[`Type`[T]])(using sp: SourcePos): F[`Type`[G[T]]]

object Meta:

  val lib: LibDep = LibDep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends Meta[[A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A]]:

    def TYPEEXPR[G[_], T](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T]]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[T]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(Meta.lib))
      yield Type(Term.TypeLevel.App.`App[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`("dc10.scala.Statement.TypeExpr", None), a.tpe))
      