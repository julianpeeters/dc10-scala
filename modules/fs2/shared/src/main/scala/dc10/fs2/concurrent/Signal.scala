package dc10.fs2.concurrent

// import cats.data.StateT
// import dc10.cats.effect.dsl.Concurrent
import dc10.scala.*
import dc10.scala.dsl.{apply, ==>}
// import dc10.scala.compiler.{Γ, dep}
// import dc10.scala.dsl.{OBJECT}



// trait Concurrent[F[_]]:
//   def SignallingRef: F[`Type.Expr: (*→*)→*→*`[SignallingRef]]
//   // @scala.annotation.targetName("SignallingRef object")
//   // def SignallingRef: F[`Value.Expr: *`[SignallingRef.type]]
//   extension (s: `Type.Expr: (*→*)→*→*`[SignallingRef])
//     def of[G[_], A](initial: A): G[SignallingRef[G, A]]

object Concurrent:

  type SignallingRef[F[_], A]

  val lib: LibDep = LibDep("co.fs2", "fs2", "3.12.0")

  // // val impl: Concurrent[[A] =>> StateT[ErrorF, Γ, A]] =
  // //   new Concurrent[[A] =>> StateT[ErrorF, Γ, A]]:
  //   def SignallingRef: StateT[ErrorF, Γ, `Type.Expr: (*→*)→*→*`[SignallingRef]] = 
  //     for
  //       t <- StateT.pure(`Type.Var: (*→*)→*→*`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None))
  //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(lib))
  //     yield t
  //   // @scala.annotation.targetName("SignallingRef object")
  //   // def SignallingRef: StateT[ErrorF, Γ,`Value.Expr: *`[SignallingRef.type]] =
  //   //   StateT.pure(OBJECT"fs2.concurrent.SignallingRef" {
        
    //   }) 

  def SignallingRef: `Type.Expr: (*→*)→*→*`[SignallingRef] =
    `Type.Var: (*→*)→*→*`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None)

  extension (s: `Type.Expr: (*→*)→*→*`[SignallingRef])
    // def of[G[_], A](initial: `Value.Expr: *`[A])(using C: Concurrent[G]): G[SignallingRef[G, A]] =
    // def of[G[_], A](initial: `Value.Expr: *`[A])(using C: `Type.Expr: (*→*)→* *→*`[Concurrent, G]): `Value.App.1: (*→*)→*→* *→* *`[SignallingRef, G, A, A] =
    def of[G[_], A](initial: `Value.Expr: *`[A]): `Value.App.1: (*→*)→*→* *→* *`[SignallingRef, G, A, A] =
      `Value.Def.1: *→*→* * ((*→*)→*→* *→* *)`(
        0,
        `DefSym.1`("fs2.concurrent.SignallingRef.of", `Value.Val: *`(0, ValSym("initial"), initial.tpe, None)),
        initial.tpe ==> `Type.App: (*→*)→*→* *→* *`(0, SignallingRef, `Type.Var: *→*`[G](0, AliasSym("F"), None, () => Nil), initial.tpe),
        None
      ).apply(initial)