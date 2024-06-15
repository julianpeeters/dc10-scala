package dc10.scala.predef.namespace

import cats.syntax.all.given
import cats.data.StateT
import dc10.scala.ctx.{dep, ext}
import dc10.scala.{ErrorF, File, LibDep, Statement}
import dc10.scala.Symbol.Package
import java.nio.file.Path
import org.tpolecat.sourcepos.SourcePos

trait Packages[F[_]]:
  def PACKAGE[A](nme: String, files: F[A])(using sp: SourcePos): F[A]

object Packages:

  trait Mixins extends Packages[
    [A] =>> StateT[ErrorF, (Set[LibDep], List[File]), A],
  ]:
    def PACKAGE[A](nme: String, files: StateT[ErrorF, (Set[LibDep], List[File]), A])(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[File]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibDep], List[File]), ((Set[LibDep], List[File]), A)](files.runEmpty)
        ss = ms.map(s => s.copy(
          path = Path.of(nme.replace(".", "/")).resolve(s.path),
          contents = List[Statement](Statement.`package`(0, sp, Package.Basic(nme, Statement.`package`(0, sp, Package.Empty(s.contents)))))
        ))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[File])](ctx => ctx.dep(l)))
        _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[LibDep], List[File])](ctx => ctx.ext(d)))
      yield a
