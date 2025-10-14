package dc10.fs2.concurrent

// import cats.data.StateT
// import dc10.cats.effect.dsl.Concurrent
import dc10.scala.*
import dc10.scala.dsl.{apply, dot, ==>}
// import dc10.scala.compiler.{Γ, dep}
// import dc10.scala.dsl.{OBJECT}



// trait Concurrent[F[_]]:
//   def SignallingRef: F[`Type: lx_xl_x_x`[SignallingRef]]
//   // @scala.annotation.targetName("SignallingRef object")
//   // def SignallingRef: F[`Value: x`[SignallingRef.type]]
//   extension (s: `Type: lx_xl_x_x`[SignallingRef])
//     def of[G[_], A](initial: A): G[SignallingRef[G, A]]

object Concurrent:

  type SignallingRef[F[_], A]

  val lib: LibDep = LibDep("co.fs2", "fs2", "3.12.0")

  // // val impl: Concurrent[[A] =>> StateT[ErrorF, Γ, A]] =
  // //   new Concurrent[[A] =>> StateT[ErrorF, Γ, A]]:
  //   def SignallingRef: StateT[ErrorF, Γ, `Type: lx_xl_x_x`[SignallingRef]] = 
  //     for
  //       t <- StateT.pure(`Type.Var: lx_xl_x_x`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None))
  //       _ <- StateT.modifyF[ErrorF, Γ](ctx => ctx.dep(lib))
  //     yield t
  //   // @scala.annotation.targetName("SignallingRef object")
  //   // def SignallingRef: StateT[ErrorF, Γ,`Value: x`[SignallingRef.type]] =
  //   //   StateT.pure(OBJECT"fs2.concurrent.SignallingRef" {
        
    //   })

  def SignallingRef: `Type: lx_xl_x_x`[SignallingRef] =
    `Type.Var: lx_xl_x_x`[SignallingRef](0, AliasSym("fs2.concurrent.SignallingRef"), None)

  extension [G[_]] (s: `Type: lx_xl_x_x x_x`[SignallingRef, G])
    // def of[G[_], A](initial: `Value: x`[A])(using C: Concurrent[G]): G[SignallingRef[G, A]] =
    def of[A](initial: `Value: x`[A]): `Value.AppDot.1: x_x llx_xl_x_x x_x xl`[G, SignallingRef, G, A, A] =
    // def of[A](initial: `Value: x`[A]): `Value.AppDot.1: x_x x`[G, A, SignallingRef[G, A]] =
      s.dot(
        `Value.Def.1: x_x_x x lx_x llx_xl_x_x x_x xll`(
        // `Value.Def.1: x_x_x x lx_x xl`(
          0,
          `DefSym.1`("of", `Value.Val: x`(0, ValSym("initial"), initial.tpe, None)),
          initial.tpe ==> s.targ1(s(initial.tpe)),
          None
        )
      )(initial)