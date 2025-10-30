package dc10.scala.predef.types

import cats.data.StateT
import cats.implicits.given
import cats.instances.unit
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.dsl.*
import dc10.scala.internal.indentation.addIndent
// import dc10.LanguageError
// import dc10.scala.internal.indentation.addIndent

// import dc10.scala.internal.indent.addIndent

// trait TemplateTypes[F[_]]:
  // @scala.annotation.targetName("caseClass1")
  // def CASECLASS[T, A](name: String, fields: F[`Value: x`[A]]): F[(`Type: x`[T], `Value: x_x_x x x`[Function1, A, T])]
  // @scala.annotation.targetName("caseClass2")
  // def CASECLASS[T, A, B](name: String, fields: F[(`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])]): F[(`Type: x`[T], `Value: x`[(A, B) => T])]
  // def FIELD[T](nme: String, tpe: F[`Type: x`[T]]): F[`Value: x`[T]]
  // @scala.annotation.targetName("trait*")
  // def TRAIT[T](nme: String, members: F[Unit]): F[`Type: x`[T]]
  // @scala.annotation.targetName("trait*extends")
  // def TRAIT[T, A](nme: String, parent: F[`Type: x`[A]], members: F[Unit]): F[`Type: x`[T]]
  // @scala.annotation.targetName("traitx_x")
  // def TRAIT[T[_], A](nme: String, tparam: F[`Type.Var: x`[A]], members: `Type.Var: x`[A] => F[Unit]): F[`Type.Var: x_x`[T]]
  // @scala.annotation.targetName("traitlx_xl_x")
  // def TRAIT[T[_[_]], H[_]](nme: String, tparam: F[`Type.Var: x_x`[H]], members: `Type.Var: x_x`[H] => F[Unit]): F[`Type: lx_xl_x`[T]]
  // @scala.annotation.targetName("traitlx_xl_x_x")
  // def TRAIT[T[_[_], _], H[_], A](nme: String, tparamF: F[`Type.Var: x_x`[H]], tparamA: F[`Type.Var: x`[A]], members: (`Type.Var: x_x`[H], `Type.Var: x`[A]) => F[Unit]): F[`Type.Var: lx_xl_x_x`[T]]

object template:

  extension (sym: SldTrtSym)
    def apply[T]: StateT[ErrorF, Γ, `Type.Var: x`[T]] =
      for
        t <- StateT.pure(`Type.Var: x`[T](0, AliasSym(sym.nme), None))
        d <- StateT.pure(`SealedTraitDef: x`[T](t))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      yield t

  extension [T] (s: StateT[ErrorF, Γ, `Type.Var: x`[T]])
    def withCompanion[U](f: `Type.Var: x`[T] => StateT[ErrorF, Γ, U]): StateT[ErrorF, Γ, `Type.Var: x`[T]] =
      for
        t <- s 
        _ <- dc10.scala.dsl.apply(OBJECT"${t.sym.nme}") {
          f(t).as(unit)
        }
      yield t

  extension [B] (sym: TrtSym)
    // def apply[T[_], A]: StateT[ErrorF, Γ, `Type.Var: x_x`[T]] =
    //   for
    //     t <- StateT.pure(`Type.Var: x_x`[T](0, AliasSym(sym.nme), None))
    //     d <- StateT.pure(`TraitDef: x_x`[T, A](t, Nil))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield t

    // def apply[T[_], A](body: StateT[ErrorF, Γ, Unit]): StateT[ErrorF, Γ, `Type.Var: x_x`[T]] =
    //   for
    //     s <- StateT.liftF(body.runEmptyS)
    //     t <- StateT.pure(`Type.Var: x_x`[T](0, AliasSym(sym.nme), None))
    //     d <- StateT.pure(`TraitDef: x_x`[T, A](t, s._2))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //     _ <- s._1.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
    //   yield t

    def apply[T[_],  A](targ1: StateT[ErrorF, Γ, `Type.Var: x`[A]])(body: `Type.Var: x`[A] => StateT[ErrorF, Γ, B]): StateT[ErrorF, Γ, (`Type.Var: x_x`[T], B)] =
      // a =>
      for
        a <- StateT.liftF(targ1.runEmptyA)
        s <- StateT.liftF(body(a).runEmpty)
        t <- StateT.pure(`Type.Var: x_x`[T](0, AliasSym(sym.nme), None))
        d <- StateT.pure(`TraitDef: x_x`[T, A](t, a, s._1._2.map(s => s.addIndent)))
        _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
      yield (t, ???)

    // def apply[T[_], U[_], A](targ1: StateT[ErrorF, Γ, `Type.Var: x`[A]])(body: `Type.Var: x`[A] => StateT[ErrorF, Γ, B]): StateT[ErrorF, Γ, (`Type.Var: x_x`[T], `Value: x_x x`[U, A])] =
    //   // a =>
    //   for
    //     a <- StateT.liftF(targ1.runEmptyA)
    //     s <- StateT.liftF(body(a).runEmpty)
    //     t <- StateT.pure(`Type.Var: x_x`[T](0, AliasSym(sym.nme), None))
    //     d <- StateT.pure(`TraitDef: x_x`[T, A](t, a, s._1._2.map(s => s.addIndent)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //     _ <- s._1._1.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
    //   yield (t, ???)
  
    // def withSelf(f: `Type.Var: x`[T] => StateT[ErrorF, Γ, `Value.Obj: x`[T]]): StateT[ErrorF, Γ, `Value.Obj: x`[T]] =
    //   s.flatMap(t => f(t))


  // trait Mixins extends TemplateTypes[[A] =>> StateT[ErrorF, Γ, A]]:
 
    // @scala.annotation.targetName("caseClass1")
    // def CASECLASS[T, A](
    //   name: String,
    //   fields: StateT[ErrorF, Γ, `Value: x`[A]]
    // ): StateT[ErrorF, Γ, (`Type: x`[T], `Value: x_x_x x x`[Function1, A, T])] =
    //   for
    //     // (fields, a) <- StateT.liftF[ErrorF, Γ, (Γ, `Value: x`[A])](fields.runEmpty)
    //     s <- StateT.liftF[ErrorF, Γ, (Γ, `Value: x`[A])](fields.runEmpty)
    //     n <- StateT.pure(`Type.Var: x`[T](0, AliasSym(name), None))
    //     v <- StateT.liftF[ErrorF, Γ, `Value: x_x_x x x`[Function1, A, T]](
    //       s._2 match
    //         case `Value.Val: x_x_x x x`(_, sym, tpe, _) => Right(
    //           `Value.Val: x_x_x x x`(
    //             lvl = 0,
    //             sym = ValSym(name),
    //             tpe = s._2.tpe ==> n,
    //             None
    //           )
    //         )
    //         case _ => Left(List(LanguageError(s"Expected Identifier but found ${s._2}")))
    //       )
    //     d <- StateT.pure(CaseClassDef(n, s._1._2.map(s => s.addIndent), Nil))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield (n, v)

    // @scala.annotation.targetName("caseClass2")
    // def CASECLASS[T, A, B](
    //   name: String,
    //   fields: StateT[ErrorF, Γ, (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B])]
    // ): StateT[ErrorF, Γ, (`Type: x`[T], `Value: x`[(A, B) => T])] =
    //   for
    //     (fields, (a, b)) <- StateT.liftF[ErrorF, Γ, (Γ, (`Value: x`[A], `Value: x`[B]))](fields.runEmpty)
    //     n <- StateT.pure(`Type.Var: x`[T](0, name, None))
    //     v <- StateT.liftF[ErrorF, Γ, `Value: x`[(A, B) => T]](
    //       (a, b) match
    //         case (`Value.Var.Unbound.Data`(in, nme, tpe), `Value.Var.Unbound.Data`(in2, nme2, tpe2)) =>
    //           Right[List[Error], `Value: x`[(A, B) => T]](
    //             `Value.Var.Unbound.Data`(
    //               in = 0,
    //               nme = name,
    //               tpe = `Type.App[_, _, _]`(
    //                 0,
    //                 `Type.Var: x_x_x_x`(0, "=>", None),
    //                 a.tpe,
    //                 b.tpe,
    //                 n
    //               )
    //             )
    //           )
    //         case _ => Left(List(Error(s"Expected Identifier but found ${a}")))
    //       )
    //     d <- StateT.pure(Statement.`case class`(n, fields._2.map(s => s.addIndent), Nil))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield (n, v)

    // def FIELD[T](
    //   nme: String,
    //   tpe: StateT[ErrorF, Γ, `Type: x`[T]]
    // ): StateT[ErrorF, Γ, `Value: x`[T]] =
    //   for
    //     t <- StateT.liftF[ErrorF, Γ, `Type: x`[T]](tpe.runEmptyA)
    //     v <- StateT.pure(`Value.Val: x`(0, ValSym(nme), t, None))
    //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.`field`(v))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield v 
  
    // @scala.annotation.targetName("trait*")
  // def TRAIT[T](
  //   nme: String,
  //   members: StateT[ErrorF, Γ, Unit]
  // ): StateT[ErrorF, Γ, `Type: x`[T]] =
  //   for
  //     _ <- StateT.liftF[ErrorF, Γ, Γ](members.runEmptyS)
  //     t <- StateT.pure(`Type.Var: x`(0, AliasSym(nme), None))
  //     d <- StateT.pure(`TraitDef: x`(t))
  //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //   yield `Type.Var: x`(0, AliasSym(nme), None)

    // @scala.annotation.targetName("trait*extends")
    // def TRAIT[T, A](
    //   nme: String,
    //   parent: StateT[ErrorF, Γ, `Type: x`[A]],
    //   members: StateT[ErrorF, Γ, Unit]
    // ): StateT[ErrorF, Γ, `Type: x`[T]] =
    //   for
    //     p <- StateT.liftF[ErrorF, Γ, `Type: x`[A]](parent.runEmptyA)
    //     b <- StateT.liftF[ErrorF, Γ, Γ](members.runEmptyS)
    //     t <- StateT.pure(`Type.Var: x`[T](0, AliasSym(nme), None))
    //     d <- StateT.pure(`TraitDef: x`(t))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //   yield t

    // @scala.annotation.targetName("traitx_x")
    // def TRAIT[T[_], A](
    //   nme: String,
    //   tparam: StateT[ErrorF, Γ, `Type.Var: x`[A]],
    //   members: `Type.Var: x`[A] => StateT[ErrorF, Γ, Unit]
    // ): StateT[ErrorF, Γ, `Type.Var: x_x`[T]] =
    //   for
    //     a <- StateT.liftF(tparam.runEmptyA)
    //     (ds, ms) <- StateT.liftF[ErrorF, Γ, Γ](members(a).runEmptyS)
    //     t <- StateT.pure(`Type.Var: x_x`(0, nme, None, () => Nil))
    //     d <- StateT.pure(Statement.`trait`.`[_]`(t, a, None, ms.map(s => s.addIndent)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //     _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
    //   yield `Type.Var: x_x`(0, nme, None, () => Nil)

    // @scala.annotation.targetName("traitlx_xl_x")
    // def TRAIT[T[_[_]], H[_]](
    //   nme: String,
    //   tparam: StateT[ErrorF, Γ, `Type.Var: x_x`[H]],
    //   members: `Type.Var: x_x`[H] => StateT[ErrorF, Γ, Unit]
    // ): StateT[ErrorF, Γ, `Type: lx_xl_x`[T]] =
    //   for
    //     a <- StateT.liftF(tparam.runEmptyA)
    //     (ds, ms) <- StateT.liftF[ErrorF, Γ, Γ](members(a).runEmptyS)
    //     t <- StateT.pure(`Type.Var: lx_xl_x`(0, nme, None))
    //     d <- StateT.pure(Statement.`trait`.`[_[_]]`(t, a, None, ms.map(s => s.addIndent)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //     _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
    //   yield `Type.Var: lx_xl_x`(0, nme, None)

    // @scala.annotation.targetName("traitlx_xl_x_x")
    // def TRAIT[T[_[_], _], H[_], A](
    //   nme: String,
    //   tparamF: StateT[ErrorF, Γ, `Type.Var: x_x`[H]],
    //   tparamA: StateT[ErrorF, Γ, `Type.Var: x`[A]],
    //   members: (`Type.Var: x_x`[H], `Type.Var: x`[A]) => StateT[ErrorF, Γ, Unit]
    // ): StateT[ErrorF, Γ, `Type.Var: lx_xl_x_x`[T]] =
    //   for
    //     f <- StateT.liftF(tparamF.runEmptyA)
    //     a <- StateT.liftF(tparamA.runEmptyA)
    //     (ds, ms) <- StateT.liftF[ErrorF, Γ, Γ](members(f, a).runEmptyS)
    //     t <- StateT.pure(`Type.Var: lx_xl_x_x`(0, nme, None))
    //     d <- StateT.pure(Statement.`trait`.`[_[_], _]`(t, f, a, None, ms.map(s => s.addIndent)))
    //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
    //     _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(l)))
    //   yield `Type.Var: lx_xl_x_x`(0, nme, None)