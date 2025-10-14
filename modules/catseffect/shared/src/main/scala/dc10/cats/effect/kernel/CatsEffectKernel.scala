package dc10.cats.effect.kernel

import dc10.scala.*
  import dc10.scala.dsl.{apply, dot, ==>}

object CatsEffectKernel:

  type Concurrent[_[_]]
  type Resource[_[_], _]

  def Concurrent: `Type: lx_xl_x`[Concurrent] =
    `Type.Var: lx_xl_x`[Concurrent](0, AliasSym("cats.effect.Concurrent"), None)

  def Resource: `Type: lx_xl_x_x`[Resource] =
    `Type.Var: lx_xl_x_x`[Resource](0, AliasSym("cats.effect.Resource"), None)

  extension [F[_], G[_[_], _], H[_], A] (r: `Value: lx_xl_x_x x_x llx_xl_x_x x_x xl`[Resource, F, G, H, A])
    def flatMap[K[_[_]], J[_]](f: `Value: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[Function1, G, H, A, Resource, F, K, J]): `Value: lx_xl_x_x x_x llx_xl_x x_xl`[Resource, F, K, J] =
      r.dot(
        `Value.Def.1: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`(
          0,
          `DefSym.1_: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`("flatMap", `Value.Val: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(0, ValSym("f"), f.tpe.targ1 ==> Resource(r.tpe.targ1, f.tpe.targ2.targ2), None)),
          (f.tpe.targ1 ==> Resource(r.tpe.targ1, f.tpe.targ2.targ2)) ==> Resource(r.tpe.targ1, f.tpe.targ2.targ2),
          None
        )
      )(f)