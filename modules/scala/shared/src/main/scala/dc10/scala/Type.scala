package dc10.scala

sealed trait Type

sealed trait `Type: *`[+T] extends Type
sealed trait `Type: *→*`[T[_]] extends Type
sealed trait `Type: (*→*)→*`[T[_[_]]] extends Type
sealed trait `Type: (*→*→*)→*`[T[_[_, _]]] extends Type
sealed trait `Type: ((*→*)→*)→*`[T[_[_[_]]]] extends Type
sealed trait `Type: ((*→*)→*→*)→*`[T[_[_[_], _]]] extends Type
sealed trait `Type: *→*→*`[T[_, _]] extends Type
sealed trait `Type: (*→*)→*→*`[T[_[_], _]] extends Type
sealed trait `Type: *→*→*→*`[T[_, _, _]] extends Type
sealed trait `Type: (*→*)→*→*→*`[T[_[_], _, _]] extends Type
sealed trait `Type: *→*→*→*→*`[T[_, _, _, _]] extends Type
sealed trait `Type: (*→*)→*→*→*→*`[T[_[_], _, _, _]] extends Type

sealed trait `Type.Expr: *`[+T] extends `Type: *`[T]
case class `Type.Lit: *`[T](lvl: Int, nme: String) extends `Type.Expr: *`[T]
case class `Type.Var: *`[T](lvl: Int, sym: AliasSym, impl: Option[`Type: *`[T]]) extends `Type.Expr: *`[T]

sealed trait `Type.Expr: *→* *`[T[_], A] extends `Type: *`[T[A]]:
  def targ1: `Type: *`[A]
case class `Type.App: *→* *`[T[_], A](lvl: Int, tfun: `Type: *→*`[T], targ1: `Type: *`[A]) extends `Type.Expr: *→* *`[T, A]
case class `Type.Var: *→* *`[T[_], A](lvl: Int, sym: AliasSym, targ1: `Type: *`[A], impl: Option[`Type: *`[T[A]]]) extends `Type.Expr: *→* *`[T, A]

sealed trait `Type.Expr: (*→*)→* *→*`[T[_[_]], F[_]] extends `Type: *`[T[F]]:
  def targ1: `Type: *→*`[F]
case class `Type.App: (*→*)→* *→*`[T[_[_]], F[_]](lvl: Int, tfun: `Type.Expr: (*→*)→*`[T], targ1: `Type: *→*`[F]) extends `Type.Expr: (*→*)→* *→*`[T, F]
case class `Type.Var: (*→*)→* *→*`[T[_[_]], F[_]](lvl: Int, sym: AliasSym, targ1: `Type: *→*`[F], impl: Option[`Type.Expr: (*→*)→* *→*`[T, F]]) extends `Type.Expr: (*→*)→* *→*`[T, F]

sealed trait `Type.Expr: ((*→*)→*)→* (*→*)→*`[T[_[_[_]]], F[_[_]]] extends `Type: *`[T[F]]:
  def targ1: `Type: (*→*)→*`[F]
case class `Type.App: ((*→*)→*)→* (*→*)→*`[T[_[_[_]]], F[_[_]]](lvl: Int, tfun: `Type.Expr: ((*→*)→*)→*`[T], targ1: `Type.Expr: (*→*)→*`[F]) extends `Type.Expr: ((*→*)→*)→* (*→*)→*`[T, F]
case class `Type.Var: ((*→*)→*)→* (*→*)→*`[T[_[_[_]]], F[_[_]]](lvl: Int, sym: AliasSym, targ1: `Type.Expr: (*→*)→*`[F], impl: Option[`Type.Expr: ((*→*)→*)→* (*→*)→*`[T, F]]) extends `Type.Expr: ((*→*)→*)→* (*→*)→*`[T, F]

sealed trait `Type.Expr: ((*→*→*)→*)→* *→*→*`[T[_[_, _]], F[_, _]] extends `Type: *`[T[F]]:
  def targ1: `Type: *→*→*`[F]
case class `Type.App: (*→*→*)→* *→*→*`[T[_[_, _]], F[_, _]](lvl: Int, tfun: `Type.Expr: (*→*→*)→*`[T], targ1: `Type.Expr: *→*→*`[F]) extends `Type.Expr: ((*→*→*)→*)→* *→*→*`[T, F]
case class `Type.Var: (*→*→*)→* *→*→*`[T[_[_, _]], F[_, _]](lvl: Int, sym: AliasSym, targ1: `Type.Expr: *→*→*`[F], impl: Option[`Type.Expr: ((*→*→*)→*)→* *→*→*`[T, F]]) extends `Type.Expr: ((*→*→*)→*)→* *→*→*`[T, F]

sealed trait `Type.Expr: ((*→*)→*→*)→* (*→*)→*→*`[T[_[_[_], _]], F[_[_], _]] extends `Type: *`[T[F]]:
  def targ1: `Type: (*→*)→*→*`[F]
case class `Type.App: ((*→*)→*→*)→* (*→*)→*→*`[T[_[_[_], _]], F[_[_], _]](lvl: Int, tfun: `Type.Expr: ((*→*)→*→*)→*`[T], targ1: `Type.Expr: (*→*)→*→*`[F]) extends `Type.Expr: ((*→*)→*→*)→* (*→*)→*→*`[T, F]
case class `Type.Var: ((*→*)→*→*)→* (*→*)→*→*`[T[_[_[_], _]], F[_[_], _]](lvl: Int, sym: AliasSym, targ1: `Type.Expr: (*→*)→*→*`[F], impl: Option[`Type.Expr: ((*→*)→*→*)→* (*→*)→*→*`[T, F]]) extends `Type.Expr: ((*→*)→*→*)→* (*→*)→*→*`[T, F]

sealed trait `Type.Expr: *→*→* * *`[T[_,_], A, B] extends `Type: *`[T[A, B]]:
  def targ1: `Type.Expr: *`[A]
  def targ2: `Type.Expr: *`[B]
case class `Type.App: *→*→* * *`[T[_,_], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *`[B]) extends `Type.Expr: *→*→* * *`[T, A, B]
case class `Type.AppInfix: *→*→* * *`[T[_,_], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *`[B]) extends `Type.Expr: *→*→* * *`[T, A, B]
case class `Type.Var: *→*→* * *`[T[_,_], A, B](lvl: Int, sym: AliasSym, targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *`[B], impl: Option[`Type.Expr: *→*→* * *`[T, A, B]]) extends `Type.Expr: *→*→* * *`[T, A, B]

sealed trait `Type.Expr: *→*→* * (*→* *)`[F[_, _], G[_], A, B] extends `Type: *`[F[A, G[B]]]:
  def targ1: `Type.Expr: *`[A]
  def targ2: `Type.Expr: *→* *`[G, B]
case class `Type.AppInfix: *→*→* * (*→* *)`[T[_,_], G[_], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *→* *`[G, B]) extends `Type.Expr: *→*→* * (*→* *)`[T, G, A, B]

sealed trait `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[F[_, _], G[_[_], _], H[_], A, B] extends `Type: *`[F[A, G[H, B]]]:
  def targ1: `Type.Expr: *`[A]
  def targ2: `Type.Expr: (*→*)→*→* *→* *`[G, H, B]
case class `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`[T[_,_], G[_[_], _], H[_], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: (*→*)→*→* *→* *`[G, H, B]) extends `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[T, G, H, A, B]

sealed trait `Type.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F[_, _], G[_[_], _], H[_], I[_], A, B] extends `Type: *`[F[A, I[G[H, B]]]]:
  def targ2: `Type.Expr: *→* ((*→*)→*→* *→* *)`[I, G, H, B]
case class `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`[T[_,_], G[_[_], _], H[_], I[_], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *→* ((*→*)→*→* *→* *)`[I, G, H, B]) extends `Type.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[T, G, H, I, A, B]

sealed trait `Type.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends `Type: *`[F[G[A], I[H, B]]]:
  def targ1: `Type.Expr: *→* *`[G, A]
  def targ2: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]
case class `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, tfun: `Type.Expr: *→*→*`[F], targ1: `Type.Expr: *→* *`[G, A], targ2: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]) extends `Type.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F, G, H, I, A, B]

sealed trait `Type.Expr: (*→*)→*→* *→* *`[T[_[_], _], F[_], A] extends `Type: *`[T[F, A]]:
  def targ1: `Type: *→*`[F]
  def targ2: `Type: *`[A]
case class `Type.App: (*→*)→*→* *→* *`[T[_[_], _], F[_], A](lvl: Int, tfun: `Type.Expr: (*→*)→*→*`[T], targ1: `Type: *→*`[F], targ2: `Type: *`[A]) extends `Type.Expr: (*→*)→*→* *→* *`[T, F, A]
case class `Type.Var: (*→*)→*→* *→* *`[T[_[_],_], F[_], A](lvl: Int, sym: AliasSym, targ1: `Type: *→*`[F], targ2: `Type: *`[A], impl: Option[`Type: *`[T[F, A]]]) extends `Type.Expr: (*→*)→*→* *→* *`[T, F, A]

sealed trait `Type.Expr: *→* ((*→*)→*→* *→* *)`[F[_], G[_[_], _], H[_], A] extends `Type: *`[F[G[H, A]]]
case class `Type.App: *→* ((*→*)→*→* *→* *)`[G[_[_], _], F[_], H[_], A](lvl: Int, tfun: `Type.Expr: *→*`[F], targ1: `Type.Expr: (*→*)→*→* *→* *`[G, H, A]) extends `Type.Expr: *→* ((*→*)→*→* *→* *)`[F, G, H, A]

sealed trait `Type.Expr: *→*→*→* * * *`[T[_,_,_], A, B, C] extends `Type: *`[T[A, B, C]]:
  def targ1: `Type: *`[A]
  def targ2: `Type: *`[B]
  def targ3: `Type: *`[C]
case class `Type.App: *→*→*→* * * *`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.Expr: *→*→*→*`[T], targ1: `Type: *`[A], targ2: `Type: *`[B], targ3: `Type: *`[C]) extends `Type.Expr: *→*→*→* * * *`[T, A, B, C]
case class `Type.AppInfix: *→*→*→* * * *`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.Expr: *→*→*→*`[T], targ1: `Type: *`[A], targ2: `Type: *`[B], targ3: `Type: *`[C]) extends `Type.Expr: *→*→*→* * * *`[T, A, B, C]
case class `Type.Var: *→*→*→* * * *`[T[_,_,_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type: *`[A], targ2: `Type: *`[B],  targ3: `Type: *`[C], impl: Option[`Type.Expr: *→*→*→* * * *`[T, A, B, C]]) extends `Type.Expr: *→*→*→* * * *`[T, A, B, C]

sealed trait `Type.Expr: (*→*)→*→*→* (*→*) * *`[T[_[_],_,_], F[_], A, B] extends `Type: *`[T[F, A, B]]:
  def targ1: `Type: *→*`[F]
  def targ2: `Type: *`[A]
  def targ3: `Type: *`[B]
case class `Type.App: (*→*)→*→*→* (*→*) * *`[T[_[_], _, _], F[_], A, B](lvl: Int, tfun: `Type.Expr: (*→*)→*→*→*`[T], targ1: `Type: *→*`[F], targ2: `Type: *`[A], targ3: `Type: *`[B]) extends `Type.Expr: (*→*)→*→*→* (*→*) * *`[T, F, A, B]
case class `Type.Var: (*→*)→*→*→* (*→*) * *`[T[_[_], _,_], F[_], A, B](lvl: Int, sym: AliasSym, targ1: `Type: *→*`[F], targ2: `Type: *`[A], targ3: `Type: *`[B], impl: Option[`Type.Expr: (*→*)→*→*→* (*→*) * *`[T, F, A, B]]) extends `Type.Expr: (*→*)→*→*→* (*→*) * *`[T, F, A, B]

sealed trait `Type.Expr: *→*→*→*→* * * * *`[T[_,_,_,_], A, B, C, D] extends `Type: *`[T[A, B, C, D]]:
  def targ1: `Type: *`[A]
  def targ2: `Type: *`[B]
  def targ3: `Type: *`[C]
  def targ4: `Type: *`[D]
case class `Type.App: *→*→*→*→* * * * *`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.Expr: *→*→*→*→*`[T], targ1: `Type: *`[A], targ2: `Type: *`[B], targ3: `Type: *`[C], targ4: `Type: *`[D]) extends `Type.Expr: *→*→*→*→* * * * *`[T, A, B, C, D]
case class `Type.AppInfix: *→*→*→*→* * * * *`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.Expr: *→*→*→*→*`[T], targ1: `Type: *`[A], targ2: `Type: *`[B], targ3: `Type: *`[C], targ4: `Type: *`[D]) extends `Type.Expr: *→*→*→*→* * * * *`[T, A, B, C, D]
case class `Type.Var: *→*→*→*→* * * * *`[T[_,_,_,_], A, B, C, D](lvl: Int, sym: AliasSym, targ1: `Type.Expr: *`[A], targ2: `Type.Expr: *`[B],  targ3: `Type.Expr: *`[C],  targ4: `Type.Expr: *`[D], impl: Option[`Type.Expr: *→*→*→*→* * * * *`[T, A, B, C, D]]) extends `Type.Expr: *→*→*→*→* * * * *`[T, A, B, C, D]

sealed trait `Type.Expr: (*→*)→*→*→*→* (*→*) * * *`[T[_[_],_,_,_], F[_], A, B, C] extends `Type: *`[T[F, A, B, C]]:
  def targ1: `Type: *→*`[F]
  def targ2: `Type: *`[A]
  def targ3: `Type: *`[B]
  def targ4: `Type: *`[C]
case class `Type.App: (*→*)→*→*→*→* (*→*) * * *`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, tfun: `Type.Expr: (*→*)→*→*→*→*`[T], targ1: `Type.Expr: *→*`[F], targ2: `Type.Expr: *`[A], targ3: `Type.Expr: *`[B], targ4: `Type.Expr: *`[C]) extends `Type.Expr: (*→*)→*→*→*→* (*→*) * * *`[T, F, A, B, C]
case class `Type.Var: (*→*)→*→*→*→* (*→*) * * *`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type.Expr: *→*`[F], targ2: `Type.Expr: *`[A], targ3: `Type.Expr: *`[B],  targ4: `Type.Expr: *`[C], impl: Option[`Type.Expr: (*→*)→*→*→*→* (*→*) * * *`[T, F, A, B, C]]) extends `Type.Expr: (*→*)→*→*→*→* (*→*) * * *`[T, F, A, B, C]

sealed trait `Type.Expr: *→*`[T[_]] extends `Type: *→*`[T]
case class `Type.Lam: *→*`[F[_], A](lvl: Int, domain: `Type.Var: *`[A], codomain: `Type.App: *→* *`[F, A]) extends `Type.Expr: *→*`[F]
case class `Type.Var: *→*`[T[_]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: *→*`[[A] =>> T[A]]], ctors: () => List[Value]) extends `Type.Expr: *→*`[[A] =>> T[A]]

sealed trait `Type.Expr: (*→*)→*→* *→*`[T[_[_], _], F[_]] extends `Type: *→*`[[A] =>> T[F, A]]:
  def tfun: `Type.Expr: (*→*)→*→*`[T]
  def targ1: `Type.Expr: *→*`[F]
case class `Type.App: (*→*)→*→* *→*`[T[_[_], _], F[_]](lvl: Int, tfun: `Type.Expr: (*→*)→*→*`[T], targ1: `Type.Expr: *→*`[F]) extends `Type.Expr: (*→*)→*→* *→*`[T, F]
case class `Type.Var: (*→*)→*→* *→*`[T[_[_],_], F[_]](lvl: Int, sym: AliasSym, tfun: `Type.Expr: (*→*)→*→*`[T], targ1: `Type.Expr: *→*`[F], impl: Option[`Type: *→*`[[A] =>> T[F, A]]]) extends `Type.Expr: (*→*)→*→* *→*`[T, F]

sealed trait `Type.Expr: *→*→* * ((*→*)→*→* *→*)`[F[_, _], G[_[_], _], H[_], A] extends `Type: *→*`[[B] =>> F[A, G[H, B]]]:
  def targ1: `Type.Expr: *`[A]
  def targ2: `Type.Expr: (*→*)→*→* *→*`[G, H]
case class `Type.AppInfix: *→*→* * ((*→*)→*→* *→*)`[T[_,_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type.Expr: *→*→*`[T], targ1: `Type.Expr: *`[A], targ2: `Type.Expr: (*→*)→*→* *→*`[G, H]) extends `Type.Expr: *→*→* * ((*→*)→*→* *→*)`[T, G, H, A]

sealed trait `Type.Expr: (*→*)→*`[T[_[_]]] extends `Type: (*→*)→*`[T]
case class `Type.Lam: (*→*)→*`[F[_[_]], G[_]](lvl: Int, domain: `Type.Var: *→*`[G], codomain: `Type.App: (*→*)→* *→*`[F, G]) extends `Type.Expr: (*→*)→*`[F]
case class `Type.Var: (*→*)→*`[T[_[_]]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: (*→*)→*`[T]]) extends `Type.Expr: (*→*)→*`[T]

sealed trait `Type.Expr: ((*→*)→*)→*`[T[_[_[_]]]] extends `Type: ((*→*)→*)→*`[T]
case class `Type.Lam: ((*→*)→*)→*`[F[_[_[_]]], G[_[_]]](lvl: Int, domain: `Type.Var: (*→*)→*`[G], codomain: `Type.App: ((*→*)→*)→* (*→*)→*`[F, G]) extends `Type.Expr: ((*→*)→*)→*`[F]
case class `Type.Var: ((*→*)→*)→*`[T[_[_[_]]]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: ((*→*)→*)→*`[T]]) extends `Type.Expr: ((*→*)→*)→*`[T]

sealed trait `Type.Expr: (*→*→*)→*`[T[_[_, _]]] extends `Type: (*→*→*)→*`[T]
case class `Type.Lam: (*→*→*)→*`[F[_[_, _]], G[_, _]](lvl: Int, domain: `Type.Var: *→*→*`[G], codomain: `Type.App: (*→*→*)→* *→*→*`[F, G]) extends `Type.Expr: (*→*→*)→*`[F]
case class `Type.Var: (*→*→*)→*`[T[_[_, _]]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: (*→*→*)→*`[T]]) extends `Type.Expr: (*→*→*)→*`[T]

sealed trait `Type.Expr: ((*→*)→*→*)→*`[T[_[_[_], _]]] extends `Type: ((*→*)→*→*)→*`[T]
case class `Type.Lam: ((*→*)→*→*)→*`[F[_[_[_], _]], G[_[_], _]](lvl: Int, domain: `Type.Var: (*→*)→*→*`[G], codomain: `Type.App: ((*→*)→*→*)→* (*→*)→*→*`[F, G]) extends `Type.Expr: ((*→*)→*→*)→*`[F]
case class `Type.Var: ((*→*)→*→*)→*`[T[_[_[_], _]]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: ((*→*)→*→*)→*`[T]]) extends `Type.Expr: ((*→*)→*→*)→*`[T]

sealed trait `Type.Expr: *→*→*`[T[_, _]] extends `Type: *→*→*`[T]
case class `Type.Lam: *→*→*`[F[_, _], A, B](lvl: Int, domain1: `Type.Var: *`[A], domain2: `Type.Var: *`[B], codomain: `Type.App: *→*→* * *`[F, A, B]) extends `Type.Expr: *→*→*`[F]
case class `Type.Var: *→*→*`[T[_, _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: *→*→*`[T]]) extends `Type.Expr: *→*→*`[T]

sealed trait `Type.Expr: (*→*)→*→*`[T[_[_], _]] extends `Type: (*→*)→*→*`[T]
case class `Type.Lam: (*→*)→*→*`[F[_[_], _], G[_], A](lvl: Int, domain1: `Type.Var: *→*`[G], domain2: `Type.Var: *`[A], codomain: `Type.App: (*→*)→*→* *→* *`[F, G, A]) extends `Type.Expr: (*→*)→*→*`[F]
case class `Type.Var: (*→*)→*→*`[T[_[_], _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: (*→*)→*→*`[T]]) extends `Type.Expr: (*→*)→*→*`[T]

sealed trait `Type.Expr: *→*→*→*`[T[_, _, _]] extends `Type: *→*→*→*`[T]
case class `Type.Lam: *→*→*→*`[F[_, _, _], A, B, C](lvl: Int, domain1: `Type.Var: *`[A], domain2: `Type.Var: *`[B], domain3: `Type.Var: *`[C], codomain: `Type.App: *→*→*→* * * *`[F, A, B, C]) extends `Type.Expr: *→*→*→*`[F]
case class `Type.Var: *→*→*→*`[T[_, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: *→*→*→*`[T]]) extends `Type.Expr: *→*→*→*`[T]

sealed trait `Type.Expr: (*→*)→*→*→*`[T[_[_], _, _]] extends `Type: (*→*)→*→*→*`[T]
case class `Type.Lam: (*→*)→*→*→*`[F[_[_], _, _], G[_], A, B](lvl: Int, domain1: `Type.Var: *→*`[G], domain2: `Type.Var: *`[A], domain3: `Type.Var: *`[B], codomain: `Type.App: (*→*)→*→*→* (*→*) * *`[F, G, A, B]) extends `Type.Expr: (*→*)→*→*→*`[F]
case class `Type.Var: (*→*)→*→*→*`[T[_[_], _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: (*→*)→*→*→*`[T]]) extends `Type.Expr: (*→*)→*→*→*`[T]

sealed trait `Type.Expr: *→*→*→*→*`[T[_, _, _, _]] extends `Type: *→*→*→*→*`[T]
case class `Type.Lam: *→*→*→*→*`[F[_, _, _, _], A, B, C, D](lvl: Int, domain1: `Type.Var: *`[A], domain2: `Type.Var: *`[B], domain3: `Type.Var: *`[C], domain4: `Type.Var: *`[D], codomain: `Type.App: *→*→*→*→* * * * *`[F, A, B, C, D]) extends `Type.Expr: *→*→*→*→*`[F]
case class `Type.Var: *→*→*→*→*`[T[_, _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: *→*→*→*→*`[T]]) extends `Type.Expr: *→*→*→*→*`[T]

sealed trait `Type.Expr: (*→*)→*→*→*→*`[T[_[_], _, _, _]] extends `Type: (*→*)→*→*→*→*`[T]
case class `Type.Lam: (*→*)→*→*→*→*`[F[_[_], _, _, _], G[_], A, B, C](lvl: Int, domain1: `Type.Var: *→*`[G], domain2: `Type.Var: *`[A], domain3: `Type.Var: *`[B], domain4: `Type.Var: *`[C], codomain: `Type.App: (*→*)→*→*→*→* (*→*) * * *`[F, G, A, B, C]) extends `Type.Expr: (*→*)→*→*→*→*`[F]
case class `Type.Var: (*→*)→*→*→*→*`[T[_[_], _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type.Expr: (*→*)→*→*→*→*`[T]]) extends `Type.Expr: (*→*)→*→*→*→*`[T]

extension (t: Type)
  def lvl: Int =
    t match
      case `Type.Lit: *`(lvl, nme) => lvl
      case `Type.Var: *`(lvl, sym, impl) => lvl
      case `Type.App: *→* *`(lvl, tfun, targ1) => lvl
      case `Type.Var: *→* *`(lvl, sym, targ1, impl) => lvl
      case `Type.App: *→* ((*→*)→*→* *→* *)`(lvl, _, _) => lvl
      case `Type.App: (*→*)→* *→*`(lvl, tfun, targ1) => lvl
      case `Type.Var: (*→*)→* *→*`(lvl, sym, targ1, impl) => lvl
      case `Type.App: ((*→*)→*)→* (*→*)→*`(lvl, tfun, targ1) => lvl
      case `Type.Var: ((*→*)→*)→* (*→*)→*`(lvl, sym, targ1, impl) => lvl
      case `Type.App: (*→*→*)→* *→*→*`(lvl, tfun, targ1) => lvl
      case `Type.Var: (*→*→*)→* *→*→*`(lvl, sym, targ1, impl) => lvl
      case `Type.App: ((*→*)→*→*)→* (*→*)→*→*`(lvl, tfun, targ1) => lvl
      case `Type.Var: ((*→*)→*→*)→* (*→*)→*→*`(lvl, sym, targ1, impl) => lvl
      case `Type.App: *→*→* * *`(lvl, tfun, targ1, targ2) => lvl
      case `Type.AppInfix: *→*→* * *`(lvl, tfun, targ1, targ2) => lvl
      case `Type.Var: *→*→* * *`(lvl, sym, targ1, targ2, impl) => lvl
      case `Type.AppInfix: *→*→* * (*→* *)`(lvl, tfun, targ1, targ2) => lvl
      case `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`(lvl, tfun, targ1, targ2) => lvl
      case `Type.AppInfix: *→*→* * ((*→*)→*→* *→*)`(lvl, tfun, targ1, targ2) => lvl
      case `Type.App: (*→*)→*→* *→* *`(lvl, tfun, targ1, targ2) => lvl
      case `Type.Var: (*→*)→*→* *→* *`(lvl, sym, targ1, targ2, impl) => lvl
      case `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`(_, _, _, _) => lvl
      case `Type.App: *→*→*→* * * *`(lvl, tfun, targ1, targ2, targ3) => lvl
      case `Type.AppInfix: *→*→*→* * * *`(lvl, tfun, targ1, targ2, targ3) => lvl
      case `Type.Var: *→*→*→* * * *`(lvl, sym, targ1, targ2, targ3, impl) => lvl
      case `Type.App: (*→*)→*→*→* (*→*) * *`(lvl, tfun, targ1, targ2, targ3) => lvl
      case `Type.Var: (*→*)→*→*→* (*→*) * *`(lvl, sym, targ1, targ2, targ3, impl) => lvl
      case `Type.App: *→*→*→*→* * * * *`(lvl, tfun, targ1, targ2, targ3, targ4) => lvl
      case `Type.AppInfix: *→*→*→*→* * * * *`(lvl, tfun, targ1, targ2, targ3, targ4) => lvl
      case `Type.Var: *→*→*→*→* * * * *`(lvl, sym, targ1, targ2, targ3, targ4, impl) => lvl
      case `Type.App: (*→*)→*→*→*→* (*→*) * * *`(lvl, tfun, targ1, targ2, targ3, targ4) => lvl
      case `Type.Var: (*→*)→*→*→*→* (*→*) * * *`(lvl, sym, targ1, targ2, targ3, targ4, impl) => lvl
      case `Type.Lam: *→*`(lvl, domain, codomain) => lvl
      case `Type.Var: *→*`(lvl, sym, impl, ctors) => lvl
      case `Type.Var: (*→*)→*→* *→*`(lvl, sym, tfun, targ1, impl) => lvl
      case `Type.App: (*→*)→*→* *→*`(lvl, a, b) => lvl
      case `Type.Lam: (*→*)→*`(lvl, domain, codomain) => lvl
      case `Type.Var: (*→*)→*`(lvl, sym, impl) => lvl
      case `Type.Lam: ((*→*)→*)→*`(lvl, domain, codomain) => lvl
      case `Type.Var: ((*→*)→*)→*`(lvl, sym, impl) => lvl
      case `Type.Lam: (*→*→*)→*`(lvl, domain, codomain) => lvl
      case `Type.Var: (*→*→*)→*`(lvl, sym, impl) => lvl
      case `Type.Lam: ((*→*)→*→*)→*`(lvl, domain, codomain) => lvl
      case `Type.Var: ((*→*)→*→*)→*`(lvl, sym, impl) => lvl
      case `Type.Lam: *→*→*`(lvl, domain1, domain2, codomain) => lvl
      case `Type.Var: *→*→*`(lvl, sym, impl) => lvl
      case `Type.Lam: (*→*)→*→*`(lvl, domain1, domain2, codomain) => lvl
      case `Type.Var: (*→*)→*→*`(lvl, sym, impl) => lvl
      case `Type.Lam: *→*→*→*`(lvl, domain1, domain2, domain3, codomain) => lvl
      case `Type.Var: *→*→*→*`(lvl, sym, impl) => lvl
      case `Type.Lam: (*→*)→*→*→*`(lvl, domain1, domain2, domain3, codomain) => lvl
      case `Type.Var: (*→*)→*→*→*`(lvl, sym, impl) => lvl
      case `Type.Lam: *→*→*→*→*`(lvl, domain1, domain2, domain3, domain4, codomain) => lvl
      case `Type.Var: *→*→*→*→*`(lvl, sym, impl) => lvl
      case `Type.Lam: (*→*)→*→*→*→*`(lvl, domain1, domain2, domain3, domain4, codomain) => lvl
      case `Type.Var: (*→*)→*→*→*→*`(lvl, sym, impl) => lvl
      case `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`(lvl, _, _, _) => lvl