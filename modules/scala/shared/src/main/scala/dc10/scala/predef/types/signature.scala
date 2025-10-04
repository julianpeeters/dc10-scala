package dc10.scala.predef.types

import cats.data.StateT
// import dc10.Dep
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.predef.calculus.function.FUNCTION1

// trait Signature[F[_]]

//   extension (sym: `DefSym.0`)
//     infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]): F[`Value.Var.Unbound.Comp.Def.0`[A, B]]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): F[`Value.Var.Unbound.Comp.Def.0`[A, B]]
//     infix def $[R](tpe: `Type.Expr: *`[R]): F[`Value.Var.Unbound.Data.Def.0`[R]]

//   extension [A, R] (sym: `DefSym.1`[A, R])
//     infix def $(tpe: `Type.Expr: *`[R]):  F[`Value.Var.Unbound.Comp.Def.1`[A, R]]

//   extension (sym: `ValSym`)
//     infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]): F[`Value.Var.Unbound.Comp.Val`[A, B]]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): F[`Value.Var.Unbound.Comp.Val`[A, B]]
//     infix def $[R](tpe: `Type.Expr: *`[R]): F[`Value.Var.Unbound.Data.Val`[R]]

//   extension (str: String)
//     infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
//     infix def $[R](tpe: `Type.Expr: *`[R]): `Value.Var.Unbound.Data.Val`[R]


object signature:

//   val impl: Signature[StateT[ErrorF, Γ, _]] =
//     new Signature[StateT[ErrorF, Γ, _]]:

    extension (sym: `DefSym.0`)
//         infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Def.0`[A, B]] =
//           for
//             v <- StateT.pure(`Value.Var.Unbound.Comp.Def.0`(0, sym, tpe, tpe.barg))
//             d <- StateT.pure(Statement.define(v))
//             _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
//           yield v

      // infix def $[A, B](tpe: `Type.Var: *→*→*`[A, B]):  StateT[ErrorF, Γ, `Value.Def.0: *`[A, B]] =
      //   for
      //     v <- StateT.pure(`Value.Var.Unbound.Comp.Def.0`(0, sym, tpe, tpe.ret))
      //     d <- StateT.pure(Statement.define(v))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield v

      infix def $[R](tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Def.0: *`[R]] =          
        for
          v <- StateT.pure(`Value.Def.0: *`(0, sym, tpe, None))
          d <- StateT.pure(`DefDef: *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension [A, R] (sym: `DefSym.1`[A, R])
      infix def $(tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Def.1: *→*→* * *`[A, R]] =
        for
          v <- StateT.pure(`Value.Def.1: *→*→* * *`(0, sym, FUNCTION1(sym.arg1.tpe, tpe), None))
          d <- StateT.pure(`DefDef: *→*→* * *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension (sym: `ValSym`)
      // infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var[_, _].*`[A, B]] =
      infix def $[T[_, _], A, B](tpe: `Type.Expr: *→*→* * *`[T, A, B]):  StateT[ErrorF, Γ, `Value.Val: *→*→* * *`[T, A, B]] =
        for
          // v <- StateT.pure(`Value.Var.Unbound.Comp.Val`(0, sym, tpe, tpe.barg))
          v <- StateT.pure(`Value.Val: *→*→* * *`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: *→*→* * *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[T[_, _], G[_[_], _], H[_], A, B](tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[T, G, H, A, B]):  StateT[ErrorF, Γ, `Value.Val: *→*→* * ((*→*)→*→* *→* *)`[T, G, H, A, B]] =
        for
          v <- StateT.pure(`Value.Val: *→*→* * ((*→*)→*→* *→* *)`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: *→*→* * ((*→*)→*→* *→* *)`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[R](tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Val: *`[R]] =
        for
          v <- StateT.pure(`Value.Val: *`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[R[_], A](tpe: `Type.Expr: *→* *`[R, A]):  StateT[ErrorF, Γ, `Value.Val: *→* *`[R, A]] =
        for
          v <- StateT.pure(`Value.Val: *→* *`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: *→* *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    
      infix def $[R[_[_], _], F[_], A](tpe: `Type.Expr: (*→*)→*→* *→* *`[R, F, A]):  StateT[ErrorF, Γ, `Value.Val: (*→*)→*→* *→* *`[R, F, A]] =
        for
          v <- StateT.pure(`Value.Val: (*→*)→*→* *→* *`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: (*→*)→*→* *→* *`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension (str: String)
      infix def $[T[_, _], A, B](tpe: `Type.AppInfix: *→*→* * *`[T, A, B]): `Value.Val: *→*→* * *`[T, A, B] =
        `Value.Val: *→*→* * *`(0, ValSym(str), tpe, None)
      // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): `Value.Var.Unbound.Comp.Val`[A, B] =
      //   `Value.Var.Unbound.Comp.Val`(0, ValSym(str), tpe, tpe.ret)
      infix def $[T](tpe: `Type.Expr: *`[T]): `Value.Val: *`[T] =
        `Value.Val: *`(0, ValSym(str), tpe, None)