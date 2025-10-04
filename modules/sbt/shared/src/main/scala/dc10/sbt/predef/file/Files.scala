package dc10.sbt.predef.file

import cats.data.StateT
import cats.syntax.all.given
import dc10.file.SourceFile
  // import dc10.scala.compiler

import dc10.sbt.compiler
import dc10.sbt.{LicenseStatement, GitignoreStatement, ReadmeStatement, RepoSym, SbtStatement}
import dc10.sbt.Build.SourceDir
import dc10.sbt.Extras.{Gitignore, License, Readme}
// import dc10.sbt.Symbol.Project.{AddSbtPlugin, CrossProject, SubProject, Root}
import dc10.scala.{ErrorF, Statement}
import cats.data.{NonEmptyList}
import dc10.LanguageError
import dc10.sbt.SbtStatement.asSbtStatement
import fs2.io.file.Path
// import java.nio.file.Path

trait Files[F[_], G[_], H[_]]:
  def BASEDIR[A](nme: String): RepoSym

  // extension (nme: StringContext)
  //   def BASEDIR(args: Any*): RepoSym
  extension (sym: RepoSym)
    def withSelf[A](files: RepoSym => F[A]): F[Unit]
  
  def BUILD[A](statements: G[A]): F[A]
  def GITIGNORE: F[Unit]
  def LICENSE: F[Unit]
  def README(text: String): F[Unit]
  def SRC[A](files: H[A]): F[SourceDir]
  // given refF: Conversion[SourceFile[NonEmptyList, SbtStatement], F[SourceFile[NonEmptyList, SbtStatement]]]

object Files:

  val impl: Files[
    [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), A],
    [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), A],
    [A] =>> StateT[ErrorF, (Set[Statement], List[SourceFile[NonEmptyList, Statement]]), A]
  ] = new Files[
      [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), A],
      [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), A],
      [A] =>> StateT[ErrorF, (Set[Statement], List[SourceFile[NonEmptyList, Statement]]), A]
    ]:
    // def BASEDIR[A](
    //   nme: String
    // )(
    //   files: Unit => StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), A]
    // ): StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), Unit] =
    //   for
    //     ((ds, ms)) <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), ((Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]))](files(()).runEmptyS)
    //     d = ds.map(d => SbtStatement.ProjectDef(AddSbtPlugin(d)))
    //     p <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](Source(List("project"," plugins.sbt"), d.toList).addParent(List(nme)))
    //     c <- if ds.isEmpty
    //       then StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((ms).map(f => f.addParent(List(nme))))
    //       else StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((ms).map(f => f.addParent(List(nme))):+p)
    //     _ <- c.traverse(f => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(f)))
    //   yield ()

    def BASEDIR[A](nme: String): RepoSym =
      RepoSym(nme)

    extension (sym: RepoSym)
      def withSelf[A](files: RepoSym => StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), A]): StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), Unit] =
        for
          s <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), ((Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]))](files(sym).runEmptyS)
         


          // d <- StateT.pure(ds.toList.map(d => SbtStatement.ProjectDef(AddSbtPlugin(d))))// match
          // d <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), NonEmptyList[SbtStatement]](ds.toList.map(d => SbtStatement.ProjectDef(AddSbtPlugin(d))) match
          //   case h :: t => Right(NonEmptyList.of(h, t*))
          //   case Nil => Left(List(LanguageError("project cannot be empty"))))
          // // p <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](Source(List("project", "plugins.sbt"), d).addParent(List(sym.nme)))
          // c <- if s._2.isEmpty//ms match
          //   case h :: t => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((ms).map(f => f.addParent(List(sym.nme))):+ Source(List("project", "plugins.sbt"), d).addParent(List(sym.nme)))
          //   case Nil => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((ms).map(f => f.addParent(List(sym.nme))))
            // then StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((s._2).map(f => f.copy(path = Path(sym.nme)/f.path)))
            // else 
          c <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[SourceFile[NonEmptyList, SbtStatement]]]((s._2).map(f => f.copy(path = Path(sym.nme)/f.path)))
          _ <- c.traverse(f => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(f)))
        yield ()

    def BUILD[A](
      statements: StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), A]
    ): StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), A] =
      for
        // ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), ((Set[SbtStatement], List[SbtStatement]), A)](statements.runEmpty)
        s <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), ((Set[SbtStatement], List[SbtStatement]), A)](statements.runEmpty)
        // ps <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), NonEmptyList[SbtStatement]](s._1._1.toList.map(d => SbtStatement.ProjectDef(AddSbtPlugin(d))) match
        ps <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), NonEmptyList[SbtStatement]](s._1._2 match
            case h :: t =>
              Right(NonEmptyList.of(h, t*))
            case Nil =>
              Left(List(LanguageError("the build cannot be empty"))))
        // _ <- ms.flatTraverse(m => m match
        //   case SbtStatement.ProjectDef(project) => project match
        //     case AddSbtPlugin(libDep) => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[Unit]]((List()))  
        //     case CrossProject(nme, src) => src.files.traverse(p => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(Source(List("shared", "src", "main", "scala") ++ p.path, p.contents.map(SbtStatement.ScalaStatement.apply)))))
        //     case Root(nme, agg) => src.files.traverse(p => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(Source(List("src", "main", "scala") ++ p.path, p.contents.map(SbtStatement.ScalaStatement.apply)))))
        //     case SubProject(nme, src) => src.files.traverse(p => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(Source(List("src", "main", "scala") ++ p.path, p.contents.map(SbtStatement.ScalaStatement.apply)))))
        //   case SbtStatement.LicenseStatement(s) => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[Unit]]((List()))
        //   case SbtStatement.GitignoreStatement(s) => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[Unit]]((List()))
        //   case SbtStatement.ReadmeStatement(s) => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[Unit]]((List()))
        //   case SbtStatement.ScalaStatement(statement) => StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), List[Unit]]((List()))
        // )
        d <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](SourceFile(Path("build.sbt"), ps))
        _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(d))
      yield s._2

    def GITIGNORE: StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), Unit] =
      for
        d <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](SourceFile(Path(".gitignore"), NonEmptyList.of(GitignoreStatement(Gitignore()))))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(d))
      yield()

    def LICENSE: StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), Unit] =
      for
        f <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](SourceFile(Path("LICENSE"), NonEmptyList.of(LicenseStatement(License()))))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(f))
      yield ()
          
    def README(text: String): StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), Unit] =
      for
        f <- StateT.pure[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]](SourceFile(Path("readme.md"), NonEmptyList.of(ReadmeStatement(Readme(text)))))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(f))
      yield ()

    def SRC[A](
      files: StateT[ErrorF, (Set[Statement], List[SourceFile[NonEmptyList, Statement]]), A]
    ): StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceDir] =
      for
        s <- StateT.liftF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), (Set[Statement], List[SourceFile[NonEmptyList, Statement]])](files.runEmptyS)
        cs = s._2.map(f => f.copy(
          path = Path("src/main/scala")/f.path, 
          contents = f.contents.map(s => s.asSbtStatement)
        ))
        // _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.dep(l)))
        _ <- cs.traverse_(f => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(f)))
      yield SourceDir(List("."), s._1) // s.map(f => f.copy(path = f.path ++ List("src", "main", "scala"))))

    // given refF: Conversion[SourceFile[NonEmptyList, SbtStatement], StateT[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]]), SourceFile[NonEmptyList, SbtStatement]]] =
    //   v => StateT.pure(v)