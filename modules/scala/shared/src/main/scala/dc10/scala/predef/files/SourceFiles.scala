package dc10.scala.predef.files

import cats.data.{NonEmptyList, StateT}
import cats.syntax.all.given
import dc10.CompilerError
import dc10.file.SourceFile
import dc10.scala.{*, given}
import dc10.scala.compiler.{Γ, Δ}
import fs2.io.file.Path

trait SourceFiles[F[_], G[_]]:
  
  extension (sym: FileSym)
    def apply[A](statements: G[A]): F[A]
    def withSelf[A](statements: FileSym => G[A]): F[A]

object SourceFiles:

  private def getPackage(ss: List[Statement]): List[String] =
    ss.headOption.fold(Nil): s =>
      s match
        case PackageDef(nme, contents) => nme
        case _ => Nil

  val impl: SourceFiles[
    [X] =>> StateT[ErrorF, Δ, X],
    [X] =>> StateT[ErrorF, Γ, X]
  ] =
    new SourceFiles[
      [X] =>> StateT[ErrorF, Δ, X],
      [X] =>> StateT[ErrorF, Γ, X]
    ]:
      
      extension (sym: FileSym)
        def apply[A](
          statements: StateT[ErrorF, Γ, A]
        ): StateT[ErrorF, Δ, A] =
          for
            s <- StateT.liftF[ErrorF, Δ, (Γ, A)](statements.runEmpty)
            n <- StateT.pure(getPackage(s._1._2).foldLeft(Path(""))((acc, p) => acc / p))
            d <- StateT.liftF[ErrorF, Δ, SourceFile[NonEmptyList, Statement]](
                NonEmptyList.fromList(s._1._2).fold(Left(List(CompilerError("Expected at least one statement")))): l =>
                  Right(SourceFile(n / s"${sym.nme}.scala", l))
              )
            _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, Δ](ctx => ctx.dep(l)))
            _ <- StateT.modifyF[ErrorF, Δ](ctx => ctx.ext(d))
          yield s._2

        def withSelf[A](
          statements: FileSym => StateT[ErrorF, Γ, A]
        ): StateT[ErrorF, Δ, A] = 
          for
            s <- StateT.liftF[ErrorF, Δ, (Γ, A)](statements(sym).runEmpty)
            n <- StateT.pure(getPackage(s._1._2).foldLeft(Path(""))((acc, p) => acc / p))
            d <- StateT.liftF[ErrorF, Δ, SourceFile[NonEmptyList, Statement]](
                NonEmptyList.fromList(s._1._2).fold(Left(List(CompilerError("Expected at least one statement")))): l =>
                  Right(SourceFile(n / s"${sym.nme}.scala", l))
              )
            _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, Δ](ctx => ctx.dep(l)))
            _ <- StateT.modifyF[ErrorF, Δ](ctx => ctx.ext(d))
          yield s._2

      // extension (nme: StringContext)
      //   def file_(): FileSym = FileSym(nme.parts.mkString)

      // extension (sym: FileSym)
      //   def apply[A](
      //     statements: StateT[ErrorF, Γ, A]
      //   ): StateT[ErrorF, (Set[Statement], List[Source[List, Statement]]), A] =
      //     for
      //       ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[Statement], List[Source[List, Statement]]), (Γ, A)](statements.runEmpty)
      //       d <- StateT.pure[ErrorF, (Set[Statement], List[Source[List, Statement]]), Source[List, Statement]](Source(List(sym.nme), ms))
      //       _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[Statement], List[Source[List, Statement]])](ctx => ctx.dep(l)))
      //       _ <- StateT.modifyF[ErrorF, (Set[Statement], List[Source[List, Statement]])](ctx => ctx.ext(d))
      //     yield a