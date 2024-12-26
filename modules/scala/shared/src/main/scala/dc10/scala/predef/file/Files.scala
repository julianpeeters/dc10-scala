package dc10.scala.predef.file

import cats.data.StateT
import cats.syntax.all.given
import dc10.File
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import java.nio.file.Path

trait Files[F[_], G[_]]:
  def FILE[A](nme: String, statements: G[A]): F[A]
  
object Files:

  trait Mixins extends Files[
    StateT[ErrorF, (Set[LibDep], List[File[Statement]]), _],
    StateT[ErrorF, (Set[LibDep], List[Statement]), _]
  ]:

    def FILE[A](
      nme: String,
      statements: StateT[ErrorF, (Set[LibDep], List[Statement]), A]
    ): StateT[ErrorF, (Set[LibDep], List[File[Statement]]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[LibDep], List[File[Statement]]), ((Set[LibDep], List[Statement]), A)](statements.runEmpty)
        n <- StateT.pure[ErrorF, (Set[LibDep], List[File[Statement]]), Path](Path.of(nme))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[File[Statement]]), File[Statement]](File(n, ms))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[File[Statement]])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[File[Statement]])](ctx => ctx.ext(d))
      yield a