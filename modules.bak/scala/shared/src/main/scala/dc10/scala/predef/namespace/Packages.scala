package dc10.scala.predef.namespace

import cats.data.StateT
import cats.syntax.all.given
import dc10.File
import dc10.scala.{ErrorF, Dep, Statement, compiler}
import java.nio.file.Path

trait Packages[F[_]]:
  def PACKAGE[A](nme: String, files: F[A]): F[A]

// object Packages:

//   trait Mixins extends Packages[
//     StateT[ErrorF, (Set[Statement], List[File[Statement]]), _],
//   ]:
//     def PACKAGE[A](nme: String, files: StateT[ErrorF, (Set[Statement], List[File[Statement]]), A]): StateT[ErrorF, (Set[Statement], List[File[Statement]]), A] =
//       for
//         ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[Statement], List[File[Statement]]), ((Set[Statement], List[File[Statement]]), A)](files.runEmpty)
//         ss = ms.map(s => s.copy(
//           path = Path.of(nme.replace(".", "/")).resolve(s.path),
//           contents = List[Statement](Statement.`package`(Some(nme), s.contents))
//         ))
//         _ = println("@@ " + ds)
//         _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[Statement], List[File[Statement]])](ctx => ctx.dep(l)))
//         _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[Statement], List[File[Statement]])](ctx => ctx.ext(d)))
//       yield a
