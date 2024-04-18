package dc10.scala.predef.file

import cats.data.StateT
import cats.syntax.all.given
import dc10.scala.{ErrorF, File, Statement, Symbol}
import dc10.scala.Statement.{LibraryDependency, PackageDef}
import dc10.scala.ctx.{dep, ext}
import java.nio.file.Path
import org.tpolecat.sourcepos.SourcePos

trait Files[F[_], G[_]]:
  def FILE[A](nme: String, statements: F[A])(using sp: SourcePos): G[A]
  
object Files:

  trait Mixins extends Files[
    [A] =>> StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A],
    [A] =>> StateT[ErrorF, (Set[LibraryDependency], List[File]), A]
    ]:

    def FILE[A](
      nme: String,
      statements: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibraryDependency], List[File]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[File]), ((Set[LibraryDependency], List[Statement]), A)](statements.runEmpty)
        n <- StateT.pure[ErrorF, (Set[LibraryDependency], List[File]), Path](Path.of(nme))
        p <- StateT.pure[ErrorF, (Set[LibraryDependency], List[File]), PackageDef](PackageDef(Symbol.Package.Empty(ms), 0))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[File]), File](File(n, List(p)))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibraryDependency], List[File])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[File])](ctx => ctx.ext(d))
      yield a