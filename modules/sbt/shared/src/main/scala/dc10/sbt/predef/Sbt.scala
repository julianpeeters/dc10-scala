package dc10.sbt

import cats.data.StateT
import cats.syntax.all.given
import dc10.file.SourceFile
import dc10.sbt.compiler
import dc10.sbt.AddSbtPlugin
import dc10.sbt.Build.SourceDir
import dc10.scala.{ErrorF, LibDep, Statement}
import fs2.io.file.Path
import cats.data.NonEmptyList
// import dc10.sbt.SbtStatement.asSbtStatement

trait Sbt[F[_], G[_]]:
  def crossProject[A](nme: String, src: SourceDir): F[Project]
  def project[A](nme: String, src: SourceDir): F[Project]
  def Root[A](nme: String, src: SourceDir): F[Unit]
    
object Sbt:

  val scalaJsCross: LibDep     = LibDep("org.portable-scala", "sbt-scalajs-crossproject",      "1.3.2")
  val scalaNativeCross: LibDep = LibDep("org.portable-scala", "sbt-scala-native-crossproject", "1.3.2")
  val scalaJs: LibDep          = LibDep("org.scala-js",       "sbt-scalajs",                   "1.20.1")
  val scalaNative: LibDep      = LibDep("org.scala-native",   "sbt-scala-native",              "0.5.9")

  val impl: Sbt[
    [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), A],
    [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SourceFile[List, Statement]]), A],
    ] = new Sbt[
      [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), A],
      [A] =>> StateT[ErrorF, (Set[SbtStatement], List[SourceFile[List, Statement]]), A],
    ]:
      
    def crossProject[A](
      nme: String,
      src: SourceDir
    ): StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), Project] =
      // val s = src.copy(files = src.files.map(f => f.copy(path = Path("shared")/f.path)))
      for
        // cs = src.copy(files = src.files.map(f => f.copy(path = Path("shared")/f.path)))

        // p <- StateT.pure(Project.CrossProject(nme, src))
        p <- src.path.toString match
          case "." => StateT.pure[ErrorF, (Set[SbtStatement], List[SbtStatement]), Project](
            Project.CrossProject(nme, src.copy(files = src.files.map(f => f.copy(path = Path("shared")/f.path)))))
          case p => StateT.pure[ErrorF, (Set[SbtStatement], List[SbtStatement]), Project](
            Project.CrossProject(nme, src.copy(path = Path(p).resolve("shared"))))
        d <- StateT.pure[ErrorF, (Set[SbtStatement], List[SbtStatement]), ProjectDef](ProjectDef(p))
        // _ <- List(scalaJsCross, scalaNativeCross, scalaJs, scalaNative).toList.map(x => LibDepStatement(x)).traverse(l =>
        _ <- List(scalaJsCross, scalaNativeCross, scalaJs, scalaNative).toList.map(x => AddSbtPlugin(x)).traverse(l =>
          StateT.modifyF[ErrorF, (Set[SbtStatement], List[SbtStatement])](ctx => ctx.dep(l)))
        // _ <- s.files.traverse(l => StateT.modifyF[ErrorF, (Set[SbtStatement], List[SourceFile[NonEmptyList, SbtStatement]])](ctx => ctx.ext(l)))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SbtStatement])](ctx =>
          println("! " + d)
          println("!! " + p)
          println("!!! " + src.path)

          ctx.ext(d))
      yield p
      
    def project[A](
      nme: String,
      src: SourceDir
    ): StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), Project] =
      for
        p <- StateT.pure(Project.SubProject(nme, src))
        d <- StateT.pure[ErrorF, (Set[SbtStatement], List[SbtStatement]), ProjectDef](ProjectDef(p))
        _ <- src.deps.toList.map(x => LibDepStatement(x)).traverse(l =>
          StateT.modifyF[ErrorF, (Set[SbtStatement], List[SbtStatement])](ctx => ctx.dep(l)))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SbtStatement])](ctx => ctx.ext(d))
      yield p
      
    def Root[A](
      nme: String,
      src: SourceDir
    ): StateT[ErrorF, (Set[SbtStatement], List[SbtStatement]), Unit] =
      for
        d <- StateT.pure[ErrorF, (Set[SbtStatement], List[SbtStatement]), ProjectDef](ProjectDef(Project.Root(nme, Nil, src)))
        _ <- StateT.modifyF[ErrorF, (Set[SbtStatement], List[SbtStatement])](ctx => ctx.ext(d))
      yield ()