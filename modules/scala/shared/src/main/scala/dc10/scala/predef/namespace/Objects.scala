package dc10.scala.predef.namespace

import cats.data.StateT
// import dc10.Dep
import dc10.scala.dsl.TYPE
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.internal.indentation.addIndent
// import dc10.scala.predef.calculus.keyword.TYPE
// import dc10.Source
// import cats.data.NonEmptyList
// import dc10.CompilerError

trait Objects[F[_]]:
  // def OBJECT[T](name: String): F[`Value: x`[T]]
  // def OBJECT[T](name: String, contents: F[Unit]): F[`Value: x`[T]]
  // def OBJECT[T](name: String, parent: `Type: x`[T], contents: F[Unit]): F[`Value: x`[T]]

  // extension (nme: StringContext)
  //   def OBJECT(): LzySym


  extension [B] (sym: ObjSym)
    // def apply[A](contents: F[A]): F[`Value.Var.Bound.Data.Obj`[A]]
    def apply[A](contents: F[B]): F[`Value.Obj: x`[A]]
    infix def EXTENDS[T, A](parent: `Type: x`[T]): F[`Value.Obj: x`[T]]
    infix def EXTENDS[T, A](parent: `Type: x`[T])(contents: F[B]): F[`Value.Obj: x`[T]]

object Objects:

  val impl: Objects[[X] =>> StateT[ErrorF, Γ, X]] =
    new Objects[[X] =>> StateT[ErrorF, Γ, X]]:
      
      // def OBJECT[T](
      //   name: String
      // ): StateT[ErrorF, Γ, `Value: x`[T]] =
      //   ???
      //   // for
      //   //   t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var.Data`[T](0, type_"$name.type", None))
      //   //   v <- StateT.pure(`Value.Var.Bound.Data.Obj`(0, LzySym(name), t, None, Nil))
      //   //   d <- StateT.pure(Statement.define(v))
      //   //   _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   // yield v
        
      // def OBJECT[T](
      //   name: String,
      //   contents: StateT[ErrorF, Γ, Unit]
      // ): StateT[ErrorF, Γ, `Value: x`[T]] =
      //   ???
        // for
        //   c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
        //   t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var.Data`[T](0, type_"$name.type", None))
        //   v <- StateT.pure(`Value.Var.Bound.Data.Obj`(0, LzySym(name), t, None, c._2.map(s => s.addIndent)))
        //   d <- StateT.pure(Statement.define(v))
        //   _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        // yield v

      // def OBJECT[T](
      //   name: String,
      //   parent: `Type: x`[T],
      //   contents: StateT[ErrorF, Γ, Unit]
      // ): StateT[ErrorF, Γ, `Value: x`[T]] =
      //   for
      //     c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
      //     v <- StateT.pure(`Value.Var.Bound.Data.Obj`(0, LzySym(name), parent, Some(parent), c._2.map(s => s.addIndent)))
      //     d <- StateT.pure(Statement.define(v))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield v


      // extension (nme: StringContext)
      //   def OBJECT(): LzySym =
      //     LzySym(nme.parts.mkString)




        
      extension [B] (sym: ObjSym)
        def apply[A](
          contents: StateT[ErrorF, Γ, B]
        ): StateT[ErrorF, Γ, `Value.Obj: x`[A]] =
          // for
          //   ((ds, ms), a) <- StateT.liftF[ErrorF, (Set[Statement], List[Source[NonEmptyList, Statement]]), (Γ, A)](statements.runEmpty)
          //   // n <- StateT.pure(getPackage(ms))
          //   d <- StateT.liftF[ErrorF, (Set[Statement], List[Source[NonEmptyList, Statement]]), Source[NonEmptyList, Statement]](
          //       NonEmptyList.fromList(ms).fold(Left(List(CompilerError("Expected at least one statement")))): l =>
          //         Right(Source(n :+ sym.nme, l))
          //     )
          //   _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[Statement], List[Source[NonEmptyList, Statement]])](ctx => ctx.dep(l)))
          //   _ <- StateT.modifyF[ErrorF, (Set[Statement], List[Source[NonEmptyList, Statement]])](ctx => ctx.ext(d))
          // yield a
        //   StateT[ErrorF, Γ, `Value: x`[T]] =
          for
            c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
            t <- StateT.pure[ErrorF, Γ, `Type.Var: x`[A]](`Type.Var: x`[A](0, TYPE"${sym.nme}.type", None))
            v <- StateT.pure(`Value.Obj: x`(0, sym, t, None, c._2.map(s => s.addIndent)))
            d <- StateT.pure(ObjDef(v))
            _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
          yield v

        infix def EXTENDS[T, A](
          parent: `Type: x`[T]
        ): StateT[ErrorF, Γ, `Value.Obj: x`[T]] =
          for
            t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var: x`[T](0, TYPE"${sym.nme}.type", None))
            v <- StateT.pure(`Value.Obj: x`(0, sym, t, Some(parent), Nil))
            d <- StateT.pure(ObjDef(v))
            _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
          yield v

        infix def EXTENDS[T, A](
          parent: `Type: x`[T]
        )(
          contents: StateT[ErrorF, Γ, B]
        ): StateT[ErrorF, Γ, `Value.Obj: x`[T]] =
          for
            c <- StateT.liftF[ErrorF, Γ, Γ](contents.runEmptyS)
            t <- StateT.pure[ErrorF, Γ, `Type: x`[T]](`Type.Var: x`[T](0, TYPE"${sym.nme}.type", None))
            v <- StateT.pure(`Value.Obj: x`(0, sym, t, Some(parent), c._2.map(s => s.addIndent)))
            d <- StateT.pure(ObjDef(v))
            _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
          yield v
