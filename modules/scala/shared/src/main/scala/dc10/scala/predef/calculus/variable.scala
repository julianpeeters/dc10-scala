package dc10.scala.predef.calculus

import cats.data.StateT
// import dc10.Dep

import dc10.scala.{*, given}
import dc10.scala.compiler.Γ

// import dc10.scala.internal.definition.*
// import dc10.scala.internal.implementation.implement
// import dc10.scala.predef.types.primitive.__
// import dc10.scala.predef.calculus.function.*

trait variable[F[_]]:

  extension (sym: AliasSym)
    def apply[T]: F[`Type.Var: *`[T]]
    // def apply[G[_], A](targ: __): F[`Type.Var: *`[G[A]]]
    // def apply[G[_]](underscore: __): F[`Type.Var: *→*`[G]]






  
  // @scala.annotation.targetName("*")
  // def TYPE[T](nme: String): F[`Type.Var.Data`[T]]
  
  // @scala.annotation.targetName("*→*")
  // def TYPE[G[_], A](nme: String, tparam: F[`Type.Var: *`[A]]): F[`Type.Var: *→*`[G]]
  
  // @scala.annotation.targetName("(*→*)→*")
  // def TYPE[G[_[_]], H[_]](nme: String, tparam: F[`Type.Var: *→*`[H]]): F[`Type.Var: (*→*)→*`[G]]
  
  // @scala.annotation.targetName("(*→*)→*→*")
  // def TYPE[G[_[_], _], H[_], A](nme: String, tparamF: F[`Type.Var: *→*`[H]], targA: F[`Type.Var: *`[A]]): F[`Type.Var: (*→*)→*→*`[G]]
  
  // def VAL[T](
  //   // value: `Value.Var.Unbound.Data`[T]
  //   // nme: String, tpe: `Type.Expr: *`[T]
  //   nme: StringContext
  // ): F[`Value.Var.Unbound.Data`[T]]

  // extension (nme: StringContext)
  //   def def_(): `DefSym.0`
  //   def VAL(): `ValSym`

  // extension (nme: StringContext)
  //   // def TYPE[T](): `Type.Var.Data`[T]
  //   def type_(args: Any*): AliasSym
  //   // def typer_(): AliasSym


  // extension (sym: `DefSym.0`)
  //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]):  F[`Value.Var.Unbound.Comp.Def`[A, B]]
  //   infix def $[A, B](tpe: `Type.Var.Comp`[A, B]):  F[`Value.Var.Unbound.Comp.Def`[A, B]]
  //   infix def $[R](tpe: `Type.Expr: *`[R]):  F[`Value.Var.Unbound.Data.Def`[R]]


  // extension (sym: `ValSym`)
  //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]):  F[`Value.Var.Unbound.Comp.Val`[A, B]]
  //   infix def $[A, B](tpe: `Type.Var.Comp`[A, B]):  F[`Value.Var.Unbound.Comp.Val`[A, B]]
  //   infix def $[R](tpe: `Type.Expr: *`[R]):  F[`Value.Var.Unbound.Data.Val`[R]]


  // extension (str: String)
  //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]):  `Value.Var.Unbound.Comp.Val`[A, B]
  //   infix def $[A, B](tpe: `Type.Var.Comp`[A, B]):  `Value.Var.Unbound.Comp.Val`[A, B]
  //   infix def $[R](tpe: `Type.Expr: *`[R]):  `Value.Var.Unbound.Data.Val`[R]


  // extension [R] (sym: `DefSym`)
  //   infix def $(tpe: `Type.Var: *`[R]): F[`Value.Var.Unbound.Data.Always`[R]]

  // extension [A, R] (sym: `DefSym.1`[A, R])
  //   infix def $(tpe: `Type.Var: *`[R]): F[`Value.Var.Unbound.Comp.Always`[A, R]]

  // extension [T] (sym: `ValSym`[T])
  //   infix def $(tpe: `Type.Var: *`[T]):  F[`Value.Var.Unbound.Data.Eager`[T]]
  //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): F[`Value.Var.Unbound.Comp.Eager`[A, B]]
    
  // extension (str: String)
  //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
  //   infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): `Value.Var.Unbound.Comp.Val`[A, B]
  //   infix def $[T](tpe: `Type.Var.Data`[T]): `Value.Var.Unbound.Data.Val`[T]
    


  // def VAL[A, B](
  //   value: `Value.Var.Unbound.Comp`[A, B]
  //   // nme: String, tpe: `Type.Expr: *`[T]
  // ): F[`Value.Var.Unbound.Comp`[A, B]]

object variable:
  

  val impl: variable[[X] =>> StateT[ErrorF, Γ, X]] =
    new variable[[X] =>> StateT[ErrorF, Γ, X]]:

      extension (sym: AliasSym)
        def apply[T]: StateT[ErrorF, Γ, `Type.Var: *`[T]] =

      // @scala.annotation.targetName("*")
      // def TYPE[T](nme: String): StateT[ErrorF, Γ, `Type.Var: *`[T]] =
          for
            t <- StateT.pure[ErrorF, Γ, `Type.Var: *`[T]](`Type.Var: *`(0, sym, None))
            d <- StateT.pure(`TypeDef: *`(t))
            _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
          yield t

        // def apply[G[_]](underscore: __): StateT[ErrorF, Γ, `Type.Var: *→*`[G]] =
        //   ???

          //           for
          //   t <- StateT.pure[ErrorF, Γ, `Type.Var.Data`[T]](`Type.Var.Data`(0, sym.nme, None))
          //   // d <- StateT.pure(Statement.`type`.Data[T](t))
          //   _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(???))
          // yield t

      // @scala.annotation.targetName("*→*")
      // def TYPE[G[_], A](
      //   nme: String,
      //   tparam: StateT[ErrorF, Γ, `Type.Var: *`[A]]
      // ): StateT[ErrorF, Γ, `Type.Var: *→*`[G]] =
      //   for
      //     a <- StateT.liftF(tparam.runEmptyA)
      //     t <- StateT.pure(`Type.Var: *→*`[G](0, nme, None, () => Nil))
      //     d <- StateT.pure(Statement.`type`.`[_]`(a, t))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield t

      // @scala.annotation.targetName("(*→*)→*")
      // def TYPE[G[_[_]], H[_]](
      //   nme: String,
      //   tparam: StateT[ErrorF, Γ, `Type.Var: *→*`[H]]
      // ): StateT[ErrorF, Γ, `Type.Var: (*→*)→*`[G]] =
      //   for
      //     a <- StateT.liftF(tparam.runEmptyA)
      //     t <- StateT.pure(`Type.Var: (*→*)→*`[G](0, nme, None))
      //     d <- StateT.pure(Statement.`type`.`[_[_]]`(a, t))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield t

      // @scala.annotation.targetName("(*→*)→*→*")
      // def TYPE[G[_[_], _], H[_], A](
      //   nme: String,
      //   targF: StateT[ErrorF, Γ, `Type.Var: *→*`[H]],
      //   targA: StateT[ErrorF, Γ, `Type.Var: *`[A]]
      // ): StateT[ErrorF, Γ, `Type.Var: (*→*)→*→*`[G]] =
      //   for
      //     f <- StateT.liftF(targF.runEmptyA)
      //     a <- StateT.liftF(targA.runEmptyA)
      //     t <- StateT.pure(`Type.Var: (*→*)→*→*`[G](0, nme, None))
      //     d <- StateT.pure(Statement.`type`.`[_[_], _]`[G, H, A](f, a, t))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield t

      // def VAL[T](
      //   value: `Value.Var.Unbound.Data`[T]
      //   // nme: String,
      //   // tpe: `Type.Expr: *`[T]
      // ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
      //   for
      //     // v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, tpe))
      //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.define(value))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield value

      // def VAL[T](
      //   // value: `Value.Var.Unbound.Data`[T]
      //   // nme: String,
      //   // tpe: `Type.Expr: *`[T]
      //   nme: StringContext
      // ): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
      //   for
      //     // v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, value.sym, value.tpe))
      //     v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, ValSym(nme.toString()), ???))
      //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.define(v))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield v


      // def VAL[A, B](
      //   value: `Value.Var.Unbound.Comp`[A, B]
      //   // nme: String,
      //   // tpe: `Type.Expr: *`[T]
      // ): StateT[ErrorF, Γ, `Value.Var.Unbound.Comp`[A, B]] =
      //   for
      //     // v <- StateT.pure(`Value.Var.Unbound.Data`[T](0, nme, tpe))
      //     d <- StateT.pure[ErrorF, Γ, Statement](Statement.define(value))
      //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //   yield value



      // extension (nme: StringContext)
      //   def def_(): `DefSym.0` =
      //     `DefSym.0`(nme.parts.mkString)

      //   def VAL(): ValSym =
      //     ValSym(nme.parts.mkString)

      
      // extension (nme: StringContext)
      //   // def type_(): AliasSym =
      //   //   AliasSym(nme.parts.mkString)
      //   def type_(args: Any*): AliasSym =
      //     AliasSym(nme.raw(args*))

        // def TYPE[T](): StateT[ErrorF, Γ, `Type.Var.Data`[T]] =
        //   for
        //     t <- StateT.pure(`Type.Var.Data`[T](0, nme.parts.mkString, None))
        //     d <- StateT.pure(Statement.define(t))
        //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
        //   yield t


      // extension [R] (sym: `DefSym`)
      //   infix def $(tpe: `Type.Var: *`[R]): StateT[ErrorF, Γ, `Value.Var.Unbound.Data.Always`[R]] =
      //     for
      //       v <- StateT.pure(`Value.Var.Unbound.Data.Always`(0, sym, tpe))
      //       d <- StateT.pure(v.define)
      //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //     yield v

      // extension [A, R] (sym: `DefSym.1`[A, R])
      //   infix def $(tpe: `Type.Var: *`[R]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Always`[A, R]] =
      //     for
      //       v <- StateT.pure(`Value.Var.Unbound.Comp.Always`(0, sym, FUNCTION1(sym.arg1.tpe, tpe)))
      //       d <- StateT.pure(v.define)
      //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //     yield v


      // extension (sym: `DefSym`)
      //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Def`[A, B]] =
      //     for
      //       v <- StateT.pure(`Value.Var.Unbound.Comp.Def`(0, sym, tpe, tpe.barg))
      //       d <- StateT.pure(Statement.define(v))
      //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //     yield v

      //   infix def $[A, B](tpe: `Type.Var.Comp`[A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Def`[A, B]] =
      //     for
      //       v <- StateT.pure(`Value.Var.Unbound.Comp.Def`(0, sym, tpe, tpe.ret))
      //       d <- StateT.pure(Statement.define(v))
      //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
      //     yield v

        // infix def $[R](tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Data.Def`[R]] =


        // infix def $[R](tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Data.Def`[R]] =
        //   sym match
        //     case `DefSym.0`(nme) => 
        //     case `DefSym.1`(nme, arg1) =>
          
        //   for
        //     v <- StateT.pure(`Value.Var.Unbound.Data.Def`(0, sym, tpe))
        //     // d <- StateT.pure(Statement.define(v))
        //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(v))
        //   yield v
      

  //     extension (sym: `ValSym`)
  //       infix def $[A, B](tpe: `Type.AppInfix: *→*→* * *`[Function1, A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Val`[A, B]] =
  //         for
  //           v <- StateT.pure(`Value.Var.Unbound.Comp.Val`(0, sym, tpe, tpe.barg))
  //           d <- StateT.pure(Statement.define(v))
  //           _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //         yield v

  //       infix def $[A, B](tpe: `Type.Var.Comp`[A, B]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Val`[A, B]] =
  //         for
  //           v <- StateT.pure(`Value.Var.Unbound.Comp.Val`(0, sym, tpe, tpe.ret))
  //           d <- StateT.pure(Statement.define(v))
  //           _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //         yield v

  //       infix def $[R](tpe: `Type.Expr: *`[R]):  StateT[ErrorF, Γ, `Value.Var.Unbound.Data.Val`[R]] =
  //         for
  //           v <- StateT.pure(`Value.Var.Unbound.Data.Val`(0, sym, tpe))
  //           d <- StateT.pure(Statement.define(v))
  //           _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //         yield v





  //     extension (str: String)
  //       infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): `Value.Var.Unbound.Comp.Val`[A, B] =
  //         `Value.Var.Unbound.Comp.Val`(0, ValSym(str), tpe, tpe.barg)
  //       infix def $[A, B](tpe: `Type.Var.Comp`[A, B]): `Value.Var.Unbound.Comp.Val`[A, B] =
  //         `Value.Var.Unbound.Comp.Val`(0, ValSym(str), tpe, tpe.ret)
  //       infix def $[T](tpe: `Type.Expr: *`[T]): `Value.Var.Unbound.Data.Val`[T] =
  //         `Value.Var.Unbound.Data.Val`(0, ValSym(str), tpe)











  //     //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Eager`[A, B]] =
  //     //     for
  //     //       v <- StateT.pure(`Value.Var.Unbound.Comp.Eager`(0, sym, tpe))
  //     //       d <- StateT.pure(v.define)
  //     //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //     //     yield v
          
  //     // extension (str: String)
  //     //   infix def $[T](tpe: `Type.Var: *`[T]):  `Value.Var.Unbound.Data.Eager`[T] =
  //     //     `Value.Var.Unbound.Data.Eager`(0, ValSym(str), tpe)
  //     //   infix def $[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): `Value.Var.Unbound.Comp.Eager`[A, B] =
  //     //     `Value.Var.Unbound.Comp.Eager`(0, ValSym(str), tpe)



  //     // extension (sym: Symbol)
  //     //   infix def |:[T](tpe: `Type.Var: *`[T]): StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]] =
  //     //     for
  //     //       v <- StateT.pure(sym match
  //     //         case DefSym(nme, args) =>
  //     //           args match
  //     //             case _ :: _ => `Value.Var.Unbound.Comp`(0, sym, )
  //     //             case Nil => `Value.Var.Unbound.Data`(0, sym, tpe)
                
  //     //         case ValSym(nme) =>
            
              
  //     //           `Value.Var.Unbound.Data`(0, sym, tpe)
  //     //         )
  //     //       d <- StateT.pure(Statement.`def`.`1`(v, tpe, None, ???))
  //     //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //     //     yield v
  //     //   infix def |:[A, B](tpe: `Type.AppInfix[_, _]`[Function1, A, B]): StateT[ErrorF, Γ, `Value.Var.Unbound.Comp`[A, B]] =
  //     //     for
  //     //       v <- StateT.pure(`Value.Var.Unbound.Comp`(0, sym, tpe))
  //     //       d <- StateT.pure(Statement.define(v))
  //     //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //     //     yield v

  // object assignment:

  //   extension [T] (lhs: StateT[ErrorF, Γ, `Type.Var.Data`[T]])
  //     def :=(
  //       rhs: `Type.Expr: *`[T]
  //     ): StateT[ErrorF, Γ, `Type.Var.Data`[T]] =
  //       for
  //         l <- StateT.liftF(lhs.runEmptyA)
  //         t <- StateT.pure(l.implement(rhs))
  //         d <- StateT.pure(Statement.`type`[T](t))
  //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //       yield t

  //   extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Def.1`[A, B]])
  //     @scala.annotation.targetName("assignDef1")
  //     def :=(
  //       rhs: `Value.Var`[A] => `Value.Expr: *`[B]
  //     ): StateT[ErrorF, Γ, `Value.Var.Bound.Comp.Def.1`[A, B]] =
  //       for
  //         l <- StateT.liftF(lhs.runEmptyA)
  //         v <- StateT.pure(l.implement(rhs(l.sym.arg1)))
  //         d <- StateT.pure(Statement.define(v))
  //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //       yield v
  
  //   extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Comp.Val`[A, B]])
  //     @scala.annotation.targetName("assignVal1")
  //     def :=(
  //       rhs: `Value.Lam.1: *→*→* * *`[A, B]
  //     ): StateT[ErrorF, Γ, `Value.Var.Bound.Comp.Val`[A, B]] =
  //       for
  //         l <- StateT.liftF(lhs.runEmptyA)
  //         v <- StateT.pure(l.implement(rhs))
  //         d <- StateT.pure(Statement.define(v))
  //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //       yield v


  //   extension [T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data.Val`[T]])
  //     @scala.annotation.targetName("assignVal2")
  //     def :=(
  //       rhs: `Value.Expr: *`[T]
  //     ): StateT[ErrorF, Γ, `Value.Var.Bound.Data.Val`[T]] =
  //       for
  //         l <- StateT.liftF(lhs.runEmptyA)
  //         v <- StateT.pure(l.implement(rhs))
  //         d <- StateT.pure(Statement.define(v))
  //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //       yield v

  // //   extension [A, B] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Comp`[A, B]])
  // //     def :=(
  // //       rhs: `Value.Var.Bound.Comp`[A, B]
  // //     ): StateT[ErrorF, Γ, `Value.Var.Bound.Comp`[A, B]] =
  // //       for
  // //         l <- StateT.liftF(lhs.runEmptyA)
  // //         // s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
  // //         // (d, v) <- StateT.liftF(s.implement(rhs))
  // //         v <- StateT.pure(l.implement(rhs))
  // //         d <- StateT.pure(v.define)
  // //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  // //       yield ???
    

  // //   extension [T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]])
  // //     @scala.annotation.targetName("assign value")
  // //     def :=(
  // //       rhs: `Value.Expr: *`[T]
  // //     ): StateT[ErrorF, Γ, `Value.Var.Bound.Data`[T]] =
  // //       for
  // //         l <- StateT.liftF(lhs.runEmptyA)
  // //         // s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
  // //         // (d, v) <- StateT.liftF(s.implement(rhs))
  // //         v <- StateT.pure(l.implement(rhs))
  // //         d <- StateT.pure(v.define)
  // //         _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  // //       yield v






  //   // extension [T] (lhs: StateT[ErrorF, Γ, `Value.Var.Unbound.Data`[T]])
  //   // @scala.annotation.targetName("assign comp")
  //   // def ==(
  //   //   rhs: `Value.Var`[T]
  //   // ): StateT[ErrorF, Γ, `Value.Var.Bound.Data`[T]] =
  //   //   for
  //   //     ctx <- StateT.liftF(lhs.runEmptyS)
  //   //     s <- StateT.liftF(ctx.pop(Error("missing value declaration")))
  //   //     (d, v) <- StateT.liftF(s.implement(rhs))
  //   //     _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.ext(d))
  //   //   yield v



