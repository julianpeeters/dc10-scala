package dc10.cats.effect.kernel

import dc10.scala.*
  import dc10.scala.dsl.dot

object CatsEffectKernel:

  type Concurrent[_[_]]
  type Resource[_[_], _]

  def Concurrent: `Type: (x→x)→x`[Concurrent] =
    `Type.Var: (x→x)→x`[Concurrent](0, AliasSym("cats.effect.Concurrent"), None)

  def Resource: `Type: (x→x)→x→x`[Resource] =
    `Type.Var: (x→x)→x→x`[Resource](0, AliasSym("cats.effect.Resource"), None)

  extension [F[_], G[_[_], _], H[_], A] (r: `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[Resource, F, G, H, A])
    def flatMap[I[_[_], _], J[_[_]], K[_],  B](f: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, Resource, F, J, K]): `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[Resource, F, J, K] =
      r.dot(f)(f)