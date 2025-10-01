package dc10.scala.predef.namespace

import cats.data.StateT
import cats.data.NonEmptyList
import cats.syntax.all.given
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.CompilerError

trait Packages[F[_]]:
  // def PACKAGE[A](nme: String, files: F[A]): F[A]

  extension (sym: PkgSym)
    def apply[A](statements: F[A]): F[A]


object Packages:

  val impl: Packages[[X] =>> StateT[ErrorF, Γ, X]] =
    new Packages[[X] =>> StateT[ErrorF, Γ, X]]:
      // def PACKAGE[A](nme: String, files: StateT[ErrorF, (Set[Statement], List[Source[List, Statement]]), A]): StateT[ErrorF, (Set[Statement], List[Source[List, Statement]]), A] =
      //   for
      //     ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[Statement], List[Source[List, Statement]]), ((Set[Statement], List[Source[List, Statement]]), A)](files.runEmpty)
      //     ss = ms.map(s => s.copy(
      //       path = s.path ++ nme.split(".").toList,
      //       contents = List[Statement](Statement.`package`(Some(nme), s.contents))
      //     ))
      //     _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[Statement], List[Source[List, Statement]])](ctx => ctx.dep(l)))
      //     _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[Statement], List[Source[List, Statement]])](ctx => ctx.ext(d)))
      //   yield a


      extension (sym: PkgSym)
        def apply[A](statements: StateT[ErrorF, Γ, A]): StateT[ErrorF, Γ, A] =
          for
            s <- StateT.liftF[ErrorF, Γ, (Γ, A)](statements.runEmpty)
            // ss = ms.map(s => s.copy(
            //   path = s.path ++ nme.split(".").toList,
            //   contents = List[Statement](Statement.`package`(Some(nme), s.contents))
            // ))
            l <- s._1._2 match
              case Nil => StateT.liftF[ErrorF, Γ, NonEmptyList[Statement]](Left(List(CompilerError("packages cannot be empty"))))
              case h ::t => StateT.pure[ErrorF, Γ, NonEmptyList[Statement]](NonEmptyList(h, t))
            d <- StateT.pure(PackageDef(List(sym.nme), l))
            _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))

            _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
            // _ <- ss.traverse(d => StateT.modifyF[ErrorF, (Set[Statement], List[Source[List, Statement]])](ctx => ctx.ext(d)))
          yield s._2
