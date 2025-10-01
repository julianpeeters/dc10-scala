package dc10.scala.predef.types

import cats.data.StateT
// import dc10.Dep
import dc10.scala.*
import dc10.scala.compiler.Γ


object named:

  extension (sym: (AliasSym, `DefSym.0`))
    // def apply[T](): StateT[ErrorF, Γ, (`Type.Expr: *`[T], `Value.Expr: *`[Unit => T])] =
    //   ???
    def apply[A, T](
      field: `Value.Val: *`[A]
    ): StateT[ErrorF, Γ, (`Type: *`[T], `Value.Expr: *`[A => T])] =
      ???
      // for
      //   _ <- StateT.pure(field)
      // yield ???

      // for
      //   // (fields, a) <- StateT.liftF[ErrorF, Γ, (Γ, `Value.Expr: *`[A])](fields.runEmpty)
      //   n <- StateT.pure(`Type.Var.Data`[T](0, sym._1, None))
      //   v <- StateT.liftF[ErrorF, Γ, `Value.Expr: *`[A => T]](
      //     field match
      //       case `Value.Var.Unbound.Data.Def.1`(i, sym, arg1, tpe) =>
      //         ???
      //       //   Right[List[Error], `Value.Expr: *`[A => T]](
      //       //   `Value.Var.Unbound.Data.Def.1`(
      //       //     lvl = 0,
      //       //     sym = sym._2,
      //       //     arg1 = field,
      //       //     tpe = `Type.App[_, _]`(0, `Type.Var: *→*→*`(0, "=>", None), a.tpe, n),
      //       //   )
      //       // )
      //       case _ => ??? // Left(List(Error(s"Expected Identifier but found ${a}")))
      //     )
      //   d <- StateT.pure(Statement.`case class`(n, fields._2.map(s => s.addIndent), Nil))
      //   _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      // yield (n, v)