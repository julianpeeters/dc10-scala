package dc10.cats.effect.kernel

import dc10.scala.*

object CatsEffectKernel:

  type Concurrent[_[_]]
  type Resource[_[_], _]

  def Concurrent: `Type.Expr: (*→*)→*`[Concurrent] =
    `Type.Var: (*→*)→*`[Concurrent](0, AliasSym("cats.effect.Concurrent"), None)

  def Resource: `Type.Expr: (*→*)→*→*`[Resource] =
    `Type.Var: (*→*)→*→*`[Resource](0, AliasSym("cats.effect.Resource"), None)