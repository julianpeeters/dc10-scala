package dc10.scala.predef.namespace

import cats.syntax.all.given
import cats.data.StateT
import dc10.scala.{ErrorF, File, Statement, Symbol}
import dc10.scala.Statement.LibraryDependency
import dc10.scala.ctx.{dep, ext}
import java.nio.file.Path
import org.tpolecat.sourcepos.SourcePos

trait Packages[F[_]]:
  def PACKAGE[A](nme: String, files: F[A])(using sp: SourcePos): F[A]

object Packages:

  trait Mixins extends Packages[
    [A] =>> StateT[ErrorF, (Set[LibraryDependency], List[File]), A],
  ]:
    def PACKAGE[A](nme: String, files: StateT[ErrorF, (Set[LibraryDependency], List[File]), A])(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[File]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[File]), ((Set[LibraryDependency], List[File]), A)](files.runEmpty)
        ss = ms.map(s => s.copy(
          path = Path.of(nme.replace(".", "/")).resolve(s.path),
          contents = List[Statement](Statement.PackageDef(Symbol.Package.Basic(nme, Statement.PackageDef(Symbol.Package.Empty(s.contents), 0)), 0))
        ))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[File])](ctx => ctx.dep(l)))
        _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[File])](ctx => ctx.ext(d)))
      yield a
