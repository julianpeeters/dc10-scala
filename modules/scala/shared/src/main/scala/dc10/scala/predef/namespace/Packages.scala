package dc10.scala.predef.namespace

import cats.syntax.all.given
import cats.data.StateT
import dc10.File
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import dc10.scala.Symbol.Package
import java.nio.file.Path

trait Packages[F[_]]:
  def PACKAGE[A](nme: String, files: F[A]): F[A]

object Packages:

  trait Mixins extends Packages[
    StateT[ErrorF, (Set[LibDep], List[File[Statement]]), _],
  ]:
    def PACKAGE[A](nme: String, files: StateT[ErrorF, (Set[LibDep], List[File[Statement]]), A]): StateT[ErrorF, (Set[LibDep], List[File[Statement]]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibDep], List[File[Statement]]), ((Set[LibDep], List[File[Statement]]), A)](files.runEmpty)
        ss = ms.map(s => s.copy(
          path = Path.of(nme.replace(".", "/")).resolve(s.path),
          contents = List[Statement](Statement.`package`(0, Package.Basic(nme, Statement.`package`(0, Package.Empty(s.contents)))))
        ))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[File[Statement]])](ctx => ctx.dep(l)))
        _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[LibDep], List[File[Statement]])](ctx => ctx.ext(d)))
      yield a
