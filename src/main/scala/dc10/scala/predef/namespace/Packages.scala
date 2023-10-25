package dc10.scala.predef.namespace

import cats.implicits.*
import cats.data.StateT
import dc10.scala.{ErrorF, File, Statement, Symbol}
import dc10.scala.ctx.ext
import java.nio.file.Path
import org.tpolecat.sourcepos.SourcePos


trait Packages[F[_]]:
  def PACKAGE[A](nme: String, files: F[A])(using sp: SourcePos): F[A]

object Packages:

  trait Mixins extends Packages[
    [A] =>> StateT[ErrorF, List[File], A],
  ]:
    def PACKAGE[A](nme: String, files: StateT[ErrorF, List[File], A])(using sp: SourcePos): StateT[ErrorF, List[File], A] =
      for
        (ms, a) <- StateT.liftF[ErrorF, List[File], (List[File], A)](files.runEmpty)
        ss = ms.map(s => s.copy(
          path = Path.of(nme).resolve(s.path),
          contents = List[Statement](Statement.PackageDef(Symbol.Package.Basic(nme, Statement.PackageDef(Symbol.Package.Empty(s.contents), 0)), 0))
        ))
        _ <- ss.traverse(d => StateT.modifyF[ErrorF, List[File]](ctx => ctx.ext(d)))
      yield a
