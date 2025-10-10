package dc10.fs2.concurrent

// import cats.data.StateT
// import dc10.cats.effect.dsl.Concurrent
import dc10.scala.*
import dc10.scala.dsl.{apply, dot, ==>}
// import dc10.scala.compiler.{Γ, dep}
// import dc10.scala.dsl.{OBJECT}



// trait Concurrent[F[_]]:
//   def SignallingRef: F[`Type: (x→x)→x→x`[SignallingRef]]
//   // @scala.annotation.targetName("SignallingRef object")
//   // def SignallingRef: F[`Value: x`[SignallingRef.type]]
//   extension (s: `Type: (x→x)→x→x`[SignallingRef])
//     def of[G[_], A](initial: A): G[SignallingRef[G, A]]

object Concurrent:

  type SignallingRef[F[_], A]

  val lib: LibDep = LibDep("co.fs2", "fs2", "3.12.0")

  // // val impl: Concurrent[[A] =>> StateT[ErrorF, Γ, A]] =
  // //   new Concurrent[[A] =>> StateT[ErrorF, Γ, A]]:
  //   def SignallingRef: StateT[ErrorF, Γ, `Type: (x→x)→x→x`[SignallingRef]] = 
  //     for
  //       t <- StateT.pure(`Type.Var: (x→x)→x→x`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None))
  //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(lib))
  //     yield t
  //   // @scala.annotation.targetName("SignallingRef object")
  //   // def SignallingRef: StateT[ErrorF, Γ,`Value: x`[SignallingRef.type]] =
  //   //   StateT.pure(OBJECT"fs2.concurrent.SignallingRef" {
        
    //   })

  def SignallingRef: `Type: (x→x)→x→x`[SignallingRef] =
    `Type.Var: (x→x)→x→x`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None)

  extension [G[_]] (s: `Type: (x→x)→x→x x→x`[SignallingRef, G])
    // def of[G[_], A](initial: `Value: x`[A])(using C: Concurrent[G]): G[SignallingRef[G, A]] =
    def of[A](initial: `Value: x`[A]): `Value.AppDot.1: x→x ((x→x)→x→x x→x x)`[G, SignallingRef, G, A, A] =
    // def of[A](initial: `Value: x`[A]): `Value.AppDot.1: x→x x`[G, A, SignallingRef[G, A]] =
      s.dot(
        `Value.Def.1: x→x→x x (x→x ((x→x)→x→x x→x x))`(
        // `Value.Def.1: x→x→x x (x→x x)`(
          0,
          `DefSym.1`("of", `Value.Val: x`(0, ValSym("initial"), initial.tpe, None)),
          initial.tpe ==> s.targ1(s(initial.tpe)),
          None
        )
      )(initial)