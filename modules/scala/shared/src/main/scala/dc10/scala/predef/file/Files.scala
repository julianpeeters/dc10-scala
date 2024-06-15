package dc10.scala.predef.file

import cats.data.StateT
import cats.syntax.all.given
import dc10.scala.{ErrorF, File, LibDep, Statement}
import dc10.scala.Symbol.Package
import dc10.scala.ctx.{dep, ext}
import java.nio.file.Path
import org.tpolecat.sourcepos.SourcePos

trait Files[F[_], G[_]]:
  def FILE[A](nme: String, statements: F[A])(using sp: SourcePos): G[A]
  
object Files:

  trait Mixins extends Files[
    [A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A],
    [A] =>> StateT[ErrorF, (Set[LibDep], List[File]), A]
    ]:

    def FILE[A](
      nme: String,
      statements: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[File]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibDep], List[File]), ((Set[LibDep], List[Statement]), A)](statements.runEmpty)
        n <- StateT.pure[ErrorF, (Set[LibDep], List[File]), Path](Path.of(nme))
        p <- StateT.pure[ErrorF, (Set[LibDep], List[File]), Statement.`package`](Statement.`package`(0, sp, Package.Empty(ms)))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[File]), File](File(n, List(p)))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[File])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[File])](ctx => ctx.ext(d))
      yield a