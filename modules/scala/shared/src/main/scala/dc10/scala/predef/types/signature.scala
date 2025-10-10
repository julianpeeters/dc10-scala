package dc10.scala.predef.types

import cats.data.StateT
// import dc10.Dep
import dc10.scala.{*, given}
import dc10.scala.compiler.Γ
import dc10.scala.predef.calculus.function.FUNCTION1

// trait Signature[F[_]]

//   extension (sym: `DefSym.0`)
//     infix def $[A, B](tpe: `Type.AppInfix: x→x→x x x`[Function1, A, B]): F[`Value.Var.Unbound.Comp.Def.0`[A, B]]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): F[`Value.Var.Unbound.Comp.Def.0`[A, B]]
//     infix def $[R](tpe: `Type: x`[R]): F[`Value.Var.Unbound.Data.Def.0`[R]]

//   extension [A, R] (sym: `DefSym.1`[A, R])
//     infix def $(tpe: `Type: x`[R]):  F[`Value.Var.Unbound.Comp.Def.1`[A, R]]

//   extension (sym: `ValSym`)
//     infix def $[A, B](tpe: `Type.AppInfix: x→x→x x x`[Function1, A, B]): F[`Value.Var.Unbound.Comp.Val`[A, B]]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): F[`Value.Var.Unbound.Comp.Val`[A, B]]
//     infix def $[R](tpe: `Type: x`[R]): F[`Value.Var.Unbound.Data.Val`[R]]

//   extension (str: String)
//     infix def $[A, B](tpe: `Type.AppInfix: x→x→x x x`[Function1, A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
//     // infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
//     infix def $[R](tpe: `Type: x`[R]): `Value.Var.Unbound.Data.Val`[R]


object signature:

//   val impl: Signature[StateT[ErrorF, Γ, _]] =
//     new Signature[StateT[ErrorF, Γ, _]]:

    extension (sym: `DefSym.0`)
//         infix def $[A, B](tpe: `Type.AppInfix: x→x→x x x`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Def.0`[A, B]] =
//           for
//             v <- StateT.pure(`Value.Var.Unbound.Comp.Def.0`(0, sym, tpe, tpe.barg))
//             d <- StateT.pure(Statement.define(v))
//             _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
//           yield v

      // infix def $[A, B](tpe: `Type.Var: x→x→x`[A, B]):  StateT[ErrorF, Γ, `Value.Def.0: x`[A, B]] =
      //   for
      //     v <- StateT.pure(`Value.Var.Unbound.Comp.Def.0`(0, sym, tpe, tpe.ret))
      //     d <- StateT.pure(Statement.define(v))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield v

      infix def $[R](tpe: `Type: x`[R]):  StateT[ErrorF, Γ, `Value.Def.0: x`[R]] =          
        for
          v <- StateT.pure(`Value.Def.0: x`(0, sym, tpe, None))
          d <- StateT.pure(`DefDef: x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension [A, R] (sym: `DefSym.1`[A, R])
      infix def $(tpe: `Type: x`[R]):  StateT[ErrorF, Γ, `Value.Def.1: x→x→x x x`[A, R]] =
        for
          v <- StateT.pure(`Value.Def.1: x→x→x x x`(0, sym, FUNCTION1(sym.arg1.tpe, tpe), None))
          d <- StateT.pure(`DefDef: x→x→x x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension (sym: `ValSym`)

      infix def $[R](tpe: `Type: x`[R]):  StateT[ErrorF, Γ, `Value.Val: x`[R]] =
        for
          v <- StateT.pure(`Value.Val: x`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[R[_], A](tpe: `Type: x→x x`[R, A]):  StateT[ErrorF, Γ, `Value.Val: x→x x`[R, A]] =
        for
          v <- StateT.pure(`Value.Val: x→x x`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x→x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    

      // infix def $[A, B](tpe: `Type.AppInfix: x→x→x x x`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var[_, _].x`[A, B]] =
      infix def $[T[_, _], A, B](tpe: `Type: x→x→x x x`[T, A, B]):  StateT[ErrorF, Γ, `Value.Val: x→x→x x x`[T, A, B]] =
        for
          // v <- StateT.pure(`Value.Var.Unbound.Comp.Val`(0, sym, tpe, tpe.barg))
          v <- StateT.pure(`Value.Val: x→x→x x x`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x→x→x x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[T[_, _], G[_[_], _], H[_], A, B](tpe: `Type: x→x→x x ((x→x)→x→x x→x x)`[T, G, H, A, B]):  StateT[ErrorF, Γ, `Value.Val: x→x→x x ((x→x)→x→x x→x x)`[T, G, H, A, B]] =
        for
          v <- StateT.pure(`Value.Val: x→x→x x ((x→x)→x→x x→x x)`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x→x→x x ((x→x)→x→x x→x x)`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[T[_, _], G[_[_], _], H[_], I[_], A, B](tpe: `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[T, G, H, I, A, B]):  StateT[ErrorF, Γ, `Value.Val: x→x→x x (x→x ((x→x)→x→x x→x x))`[T, G, H, I, A, B]] =
        for
          v <- StateT.pure(`Value.Val: x→x→x x (x→x ((x→x)→x→x x→x x))`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x→x→x x (x→x ((x→x)→x→x x→x x))`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

      infix def $[R[_[_], _], F[_], G[_], A](tpe: `Type: x→x ((x→x)→x→x x→x x)`[F, R, G, A]):  StateT[ErrorF, Γ, `Value.Val: x→x ((x→x)→x→x x→x x)`[F, R, G, A]] =
        for
          v <- StateT.pure(`Value.Val: x→x ((x→x)→x→x x→x x)`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: x→x ((x→x)→x→x x→x x)`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v
    
      infix def $[R[_[_], _], F[_], A](tpe: `Type: (x→x)→x→x x→x x`[R, F, A]):  StateT[ErrorF, Γ, `Value.Val: (x→x)→x→x x→x x`[R, F, A]] =
        for
          v <- StateT.pure(`Value.Val: (x→x)→x→x x→x x`(0, sym, tpe, None))
          d <- StateT.pure(`ValDef: (x→x)→x→x x→x x`(v))
          _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        yield v

    extension (str: String)
      infix def $[T[_, _], A, B](tpe: `Type.AppInfix: x→x→x x x`[T, A, B]): `Value.Val: x→x→x x x`[T, A, B] =
        `Value.Val: x→x→x x x`(0, ValSym(str), tpe, None)
      infix def $[T](tpe: `Type: x`[T]): `Value.Val: x`[T] =
        `Value.Val: x`(0, ValSym(str), tpe, None)
      infix def $[T[_[_], _], F[_], A](tpe: `Type: (x→x)→x→x x→x x`[T, F, A]): `Value.Val: (x→x)→x→x x→x x`[T, F, A] =
        `Value.Val: (x→x)→x→x x→x x`(0, ValSym(str), tpe, None)