package dc10.scala.predef.namespace

import cats.data.StateT
import dc10.scala.{ErrorF, Statement, Symbol}
import dc10.scala.Statement.{LibraryDependency, ObjectDef, TypeExpr}
import dc10.scala.ctx.ext

trait Objects[F[_]]:
  def OBJECT[T, A](name: String, contents: F[A]): F[List[Statement]]
  def OBJECT[T, A](name: String, parent: TypeExpr[T], contents: F[A]): F[List[Statement]]

object Objects:

  trait Mixins extends Objects[[A] =>> StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]]:
    
    def OBJECT[T, A](name: String, contents: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[Statement]), (Set[LibraryDependency], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), Symbol.Object[T]](Symbol.Object(None, name, None, c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ObjectDef](ObjectDef(o, 0))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield c._2

    def OBJECT[T, A](name: String, parent: TypeExpr[T], contents: StateT[ErrorF, (Set[LibraryDependency], List[Statement]), A]): StateT[ErrorF, (Set[LibraryDependency], List[Statement]), List[Statement]] =
      for
        c <- StateT.liftF[ErrorF, (Set[LibraryDependency], List[Statement]), (Set[LibraryDependency], List[Statement])](contents.runEmptyS)
        o <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), Symbol.Object[T]](Symbol.Object(None, name, Some(parent.tpe), c._2.map(s => s.addIndent)))
        d <- StateT.pure[ErrorF, (Set[LibraryDependency], List[Statement]), ObjectDef](ObjectDef(o, 0))
        _ <- StateT.modifyF[ErrorF, (Set[LibraryDependency], List[Statement])](ctx => ctx.ext(d))
      yield c._2
