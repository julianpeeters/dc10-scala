package dc10.cats.effect.kernel

import cats.data.StateT
// import dc10.cats.effect.CatsEffect
import dc10.scala.*
// import dc10.scala.compiler

type Resource[_[_], _]

trait CatsEffectKernel[F[_]]:
  def Resource: `Type.Expr: (*→*)→*→*`[Resource]

object CatsEffectKernel:

  val impl: CatsEffectKernel[[A] =>> StateT[ErrorF, (Set[Statement], List[Statement]), A]] =
    new CatsEffectKernel[[A] =>> StateT[ErrorF, (Set[Statement], List[Statement]), A]]:

      def Resource: `Type.Expr: (*→*)→*→*`[Resource] =
        `Type.Var: (*→*)→*→*`[Resource](0, AliasSym("cats.effect.Resource"), None)