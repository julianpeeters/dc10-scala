package dc10.scala.predef.file

import cats.data.StateT
import cats.syntax.all.given
import dc10.File
import dc10.scala.{ErrorF, Dep, Statement, compiler}
import java.nio.file.Path

trait Files[F[_], G[_]]:
  def FILE[A](nme: String, statements: G[A]): F[A]
  
object Files:

  trait Mixins extends Files[
    StateT[ErrorF, (Set[Statement], List[File[Statement]]), _],
    StateT[ErrorF, Γ, _]
  ]:

    def FILE[A](
      nme: String,
      statements: StateT[ErrorF, Γ, A]
    ): StateT[ErrorF, (Set[Statement], List[File[Statement]]), A] =
      for
        ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[Statement], List[File[Statement]]), (Γ, A)](statements.runEmpty)
        n <- StateT.pure[ErrorF, (Set[Statement], List[File[Statement]]), Path](Path.of(nme))
        d <- StateT.pure[ErrorF, (Set[Statement], List[File[Statement]]), File[Statement]](File(n, ms))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[Statement], List[File[Statement]])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[Statement], List[File[Statement]])](ctx => ctx.ext(d))
      yield a