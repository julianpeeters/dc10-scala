package dc10.scala

sealed trait Type:
  def lvl: Int

sealed trait `Type: x`[+T] extends Type
case class `Type.Lit: x`[T](lvl: Int, nme: String) extends `Type: x`[T]
case class `Type.Var: x`[T](lvl: Int, sym: AliasSym, impl: Option[`Type: x`[T]]) extends `Type: x`[T]

sealed trait `Type: x→x x`[T[_], A] extends Type:
  def targ1: `Type: x`[A]
case class `Type.App: x→x x`[T[_], A](lvl: Int, tfun: `Type: x→x`[T], targ1: `Type: x`[A]) extends `Type: x→x x`[T, A]
case class `Type.Var: x→x x`[T[_], A](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], impl: Option[`Type: x`[T[A]]]) extends `Type: x→x x`[T, A]

sealed trait `Type: (x→x)→x x→x`[T[_[_]], F[_]] extends Type:
  def targ1: `Type: x→x`[F]
case class `Type.App: (x→x)→x x→x`[T[_[_]], F[_]](lvl: Int, tfun: `Type: (x→x)→x`[T], targ1: `Type: x→x`[F]) extends `Type: (x→x)→x x→x`[T, F]
case class `Type.Var: (x→x)→x x→x`[T[_[_]], F[_]](lvl: Int, sym: AliasSym, targ1: `Type: x→x`[F], impl: Option[`Type: (x→x)→x x→x`[T, F]]) extends `Type: (x→x)→x x→x`[T, F]

sealed trait `Type: ((x→x)→x)→x (x→x)→x`[T[_[_[_]]], F[_[_]]] extends Type:
  def targ1: `Type: (x→x)→x`[F]
case class `Type.App: ((x→x)→x)→x (x→x)→x`[T[_[_[_]]], F[_[_]]](lvl: Int, tfun: `Type: ((x→x)→x)→x`[T], targ1: `Type: (x→x)→x`[F]) extends `Type: ((x→x)→x)→x (x→x)→x`[T, F]
case class `Type.Var: ((x→x)→x)→x (x→x)→x`[T[_[_[_]]], F[_[_]]](lvl: Int, sym: AliasSym, targ1: `Type: (x→x)→x`[F], impl: Option[`Type: ((x→x)→x)→x (x→x)→x`[T, F]]) extends `Type: ((x→x)→x)→x (x→x)→x`[T, F]

sealed trait `Type: ((x→x→x)→x)→x x→x→x`[T[_[_, _]], F[_, _]] extends Type:
  def targ1: `Type: x→x→x`[F]
case class `Type.App: (x→x→x)→x x→x→x`[T[_[_, _]], F[_, _]](lvl: Int, tfun: `Type: (x→x→x)→x`[T], targ1: `Type: x→x→x`[F]) extends `Type: ((x→x→x)→x)→x x→x→x`[T, F]
case class `Type.Var: (x→x→x)→x x→x→x`[T[_[_, _]], F[_, _]](lvl: Int, sym: AliasSym, targ1: `Type: x→x→x`[F], impl: Option[`Type: ((x→x→x)→x)→x x→x→x`[T, F]]) extends `Type: ((x→x→x)→x)→x x→x→x`[T, F]

sealed trait `Type: ((x→x)→x→x)→x (x→x)→x→x`[T[_[_[_], _]], F[_[_], _]] extends Type:
  def targ1: `Type: (x→x)→x→x`[F]
case class `Type.App: ((x→x)→x→x)→x (x→x)→x→x`[T[_[_[_], _]], F[_[_], _]](lvl: Int, tfun: `Type: ((x→x)→x→x)→x`[T], targ1: `Type: (x→x)→x→x`[F]) extends `Type: ((x→x)→x→x)→x (x→x)→x→x`[T, F]
case class `Type.Var: ((x→x)→x→x)→x (x→x)→x→x`[T[_[_[_], _]], F[_[_], _]](lvl: Int, sym: AliasSym, targ1: `Type: (x→x)→x→x`[F], impl: Option[`Type: ((x→x)→x→x)→x (x→x)→x→x`[T, F]]) extends `Type: ((x→x)→x→x)→x (x→x)→x→x`[T, F]

sealed trait `Type: x→x→x x x`[T[_,_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: x`[B]
case class `Type.App: x→x→x x x`[T[_,_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B]) extends `Type: x→x→x x x`[T, A, B]
case class `Type.AppInfix: x→x→x x x`[T[_,_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B]) extends `Type: x→x→x x x`[T, A, B]
case class `Type.Var: x→x→x x x`[T[_,_], A, B](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B], impl: Option[`Type: x→x→x x x`[T, A, B]]) extends `Type: x→x→x x x`[T, A, B]

sealed trait `Type: x→x→x x (x→x x)`[F[_, _], G[_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: x→x x`[G, B]
case class `Type.AppInfix: x→x→x x (x→x x)`[T[_,_], G[_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x→x x`[G, B]) extends `Type: x→x→x x (x→x x)`[T, G, A, B]

sealed trait `Type: x→x→x x ((x→x)→x→x x→x x)`[F[_, _], G[_[_], _], H[_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: (x→x)→x→x x→x x`[G, H, B]
case class `Type.AppInfix: x→x→x x ((x→x)→x→x x→x x)`[T[_,_], G[_[_], _], H[_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: (x→x)→x→x x→x x`[G, H, B]) extends `Type: x→x→x x ((x→x)→x→x x→x x)`[T, G, H, A, B]

sealed trait `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[F[_, _], G[_[_], _], H[_], I[_], A, B] extends Type:
  def targ2: `Type: x→x ((x→x)→x→x x→x x)`[I, G, H, B]
case class `Type.AppInfix: x→x→x x (x→x ((x→x)→x→x x→x x))`[T[_,_], G[_[_], _], H[_], I[_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x→x ((x→x)→x→x x→x x)`[I, G, H, B]) extends `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[T, G, H, I, A, B]

sealed trait `Type: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends Type:
  def targ1: `Type: x→x x`[G, A]
  def targ2: `Type: (x→x)→x→x x→x x`[I, H, B]
case class `Type.AppInfix: x→x→x (x→x x) ((x→x)→x→x x→x x)`[T[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x→x x`[G, A], targ2: `Type: (x→x)→x→x x→x x`[I, H, B]) extends `Type: x→x→x (x→x x) ((x→x)→x→x x→x x)`[T, G, H, I, A, B]

sealed trait `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]]:
  def targ1: `Type: (x→x)→x→x x→x x`[G, H, A]
  def targ2: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
case class `Type.AppInfix: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, tfun: `Type: x→x→x`[F], targ1: `Type: (x→x)→x→x x→x x`[G, H, A], targ2: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]) extends `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]

sealed trait `Type: (x→x)→x→x x→x x`[T[_[_], _], F[_], A] extends Type:
  def targ1: `Type: x→x`[F]
  def targ2: `Type: x`[A]
case class `Type.App: (x→x)→x→x x→x x`[T[_[_], _], F[_], A](lvl: Int, tfun: `Type: (x→x)→x→x`[T], targ1: `Type: x→x`[F], targ2: `Type: x`[A]) extends `Type: (x→x)→x→x x→x x`[T, F, A]
case class `Type.Var: (x→x)→x→x x→x x`[T[_[_],_], F[_], A](lvl: Int, sym: AliasSym, targ1: `Type: x→x`[F], targ2: `Type: x`[A], impl: Option[`Type: x`[T[F, A]]]) extends `Type: (x→x)→x→x x→x x`[T, F, A]

sealed trait `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[T[_[_], _], F[_], G[_[_]], H[_]] extends Type:
  def targ1: `Type: x→x`[F]
  def targ2: `Type: (x→x)→x x→x`[G, H]
case class `Type.App: (x→x)→x→x x→x ((x→x)→x x→x)`[T[_[_], _], F[_], G[_[_]], H[_]](lvl: Int, tfun: `Type: (x→x)→x→x`[T], targ1: `Type: x→x`[F], targ2: `Type: (x→x)→x x→x`[G, H]) extends `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H]

sealed trait `Type: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T[_[_], _], F[_], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: x→x`[F]
  def targ2: `Type: (x→x)→x→x x→x x`[G, H, A]
case class `Type.App: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T[_[_], _], F[_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: (x→x)→x→x`[T], targ1: `Type: x→x`[F], targ2: `Type: (x→x)→x→x x→x x`[G, H, A]) extends `Type: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T, F, G, H, A]

sealed trait `Type: x→x ((x→x)→x→x x→x x)`[F[_], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: (x→x)→x→x x→x x`[G, H, A]
case class `Type.App: x→x ((x→x)→x→x x→x x)`[F[_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: x→x`[F], targ1: `Type: (x→x)→x→x x→x x`[G, H, A]) extends `Type: x→x ((x→x)→x→x x→x x)`[F, G, H, A]

// sealed trait `Type: x→x→x→x x * x`[T[_,_,_], A, B, C] extends Type
//   def targ1: `Type: x`[A]
//   def targ2: `Type: x`[B]
//   def targ3: `Type: x`[C]
// case class `Type.App: x→x→x→x x * x`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type: x→x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C]) extends `Type: x→x→x→x x * x`[T, A, B, C]
// case class `Type.AppInfix: x→x→x→x x * x`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type: x→x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C]) extends `Type: x→x→x→x x * x`[T, A, B, C]
// case class `Type.Var: x→x→x→x x * x`[T[_,_,_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B],  targ3: `Type: x`[C], impl: Option[`Type: x→x→x→x x * x`[T, A, B, C]]) extends `Type: x→x→x→x x * x`[T, A, B, C]

// sealed trait `Type: (x→x)→x→x→x (x→x) x x`[T[_[_],_,_], F[_], A, B] extends Type
//   def targ1: `Type: x→x`[F]
//   def targ2: `Type: x`[A]
//   def targ3: `Type: x`[B]
// case class `Type.App: (x→x)→x→x→x (x→x) x x`[T[_[_], _, _], F[_], A, B](lvl: Int, tfun: `Type: (x→x)→x→x→x`[T], targ1: `Type: x→x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B]) extends `Type: (x→x)→x→x→x (x→x) x x`[T, F, A, B]
// case class `Type.Var: (x→x)→x→x→x (x→x) x x`[T[_[_], _,_], F[_], A, B](lvl: Int, sym: AliasSym, targ1: `Type: x→x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B], impl: Option[`Type: (x→x)→x→x→x (x→x) x x`[T, F, A, B]]) extends `Type: (x→x)→x→x→x (x→x) x x`[T, F, A, B]

// sealed trait `Type: x→x→x→x→x x * x x`[T[_,_,_,_], A, B, C, D] extends Type
//   def targ1: `Type: x`[A]
//   def targ2: `Type: x`[B]
//   def targ3: `Type: x`[C]
//   def targ4: `Type: x`[D]
// case class `Type.App: x→x→x→x→x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type: x→x→x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C], targ4: `Type: x`[D]) extends `Type: x→x→x→x→x x * x x`[T, A, B, C, D]
// case class `Type.AppInfix: x→x→x→x→x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type: x→x→x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C], targ4: `Type: x`[D]) extends `Type: x→x→x→x→x x * x x`[T, A, B, C, D]
// case class `Type.Var: x→x→x→x→x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B],  targ3: `Type: x`[C],  targ4: `Type: x`[D], impl: Option[`Type: x→x→x→x→x x * x x`[T, A, B, C, D]]) extends `Type: x→x→x→x→x x * x x`[T, A, B, C, D]

// sealed trait `Type: (x→x)→x→x→x→x (x→x) x * x`[T[_[_],_,_,_], F[_], A, B, C] extends Type
//   def targ1: `Type: x→x`[F]
//   def targ2: `Type: x`[A]
//   def targ3: `Type: x`[B]
//   def targ4: `Type: x`[C]
// case class `Type.App: (x→x)→x→x→x→x (x→x) x * x`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, tfun: `Type: (x→x)→x→x→x→x`[T], targ1: `Type: x→x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B], targ4: `Type: x`[C]) extends `Type: (x→x)→x→x→x→x (x→x) x * x`[T, F, A, B, C]
// case class `Type.Var: (x→x)→x→x→x→x (x→x) x * x`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type: x→x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B],  targ4: `Type: x`[C], impl: Option[`Type: (x→x)→x→x→x→x (x→x) x * x`[T, F, A, B, C]]) extends `Type: (x→x)→x→x→x→x (x→x) x * x`[T, F, A, B, C]

sealed trait `Type: x→x`[T[_]] extends Type
case class `Type.Lam: x→x`[F[_], A](lvl: Int, domain: `Type.Var: x`[A], codomain: `Type.App: x→x x`[F, A]) extends `Type: x→x`[F]
case class `Type.Var: x→x`[T[_]](lvl: Int, sym: AliasSym, impl: Option[`Type: x→x`[[A] =>> T[A]]], ctors: () => List[Value]) extends `Type: x→x`[T]

sealed trait `Type: (x→x)→x→x x→x`[T[_[_], _], F[_]] extends Type:
  def tfun: `Type: (x→x)→x→x`[T]
  def targ1: `Type: x→x`[F]
case class `Type.App: (x→x)→x→x x→x`[T[_[_], _], F[_]](lvl: Int, tfun: `Type: (x→x)→x→x`[T], targ1: `Type: x→x`[F]) extends `Type: (x→x)→x→x x→x`[T, F]
case class `Type.Var: (x→x)→x→x x→x`[T[_[_],_], F[_]](lvl: Int, sym: AliasSym, tfun: `Type: (x→x)→x→x`[T], targ1: `Type: x→x`[F], impl: Option[`Type: x→x`[[A] =>> T[F, A]]]) extends `Type: (x→x)→x→x x→x`[T, F]

sealed trait `Type: x→x→x x ((x→x)→x→x x→x)`[F[_, _], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: (x→x)→x→x x→x`[G, H]
case class `Type.AppInfix: x→x→x x ((x→x)→x→x x→x)`[T[_,_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: x→x→x`[T], targ1: `Type: x`[A], targ2: `Type: (x→x)→x→x x→x`[G, H]) extends `Type: x→x→x x ((x→x)→x→x x→x)`[T, G, H, A]

sealed trait `Type: (x→x)→x`[T[_[_]]] extends Type
case class `Type.Lam: (x→x)→x`[F[_[_]], G[_]](lvl: Int, domain: `Type.Var: x→x`[G], codomain: `Type.App: (x→x)→x x→x`[F, G]) extends `Type: (x→x)→x`[F]
case class `Type.Var: (x→x)→x`[T[_[_]]](lvl: Int, sym: AliasSym, impl: Option[`Type: (x→x)→x`[T]]) extends `Type: (x→x)→x`[T]

sealed trait `Type: ((x→x)→x)→x`[T[_[_[_]]]] extends Type
case class `Type.Lam: ((x→x)→x)→x`[F[_[_[_]]], G[_[_]]](lvl: Int, domain: `Type.Var: (x→x)→x`[G], codomain: `Type.App: ((x→x)→x)→x (x→x)→x`[F, G]) extends `Type: ((x→x)→x)→x`[F]
case class `Type.Var: ((x→x)→x)→x`[T[_[_[_]]]](lvl: Int, sym: AliasSym, impl: Option[`Type: ((x→x)→x)→x`[T]]) extends `Type: ((x→x)→x)→x`[T]

sealed trait `Type: (x→x→x)→x`[T[_[_, _]]] extends Type
case class `Type.Lam: (x→x→x)→x`[F[_[_, _]], G[_, _]](lvl: Int, domain: `Type.Var: x→x→x`[G], codomain: `Type.App: (x→x→x)→x x→x→x`[F, G]) extends `Type: (x→x→x)→x`[F]
case class `Type.Var: (x→x→x)→x`[T[_[_, _]]](lvl: Int, sym: AliasSym, impl: Option[`Type: (x→x→x)→x`[T]]) extends `Type: (x→x→x)→x`[T]

sealed trait `Type: ((x→x)→x→x)→x`[T[_[_[_], _]]] extends Type
case class `Type.Lam: ((x→x)→x→x)→x`[F[_[_[_], _]], G[_[_], _]](lvl: Int, domain: `Type.Var: (x→x)→x→x`[G], codomain: `Type.App: ((x→x)→x→x)→x (x→x)→x→x`[F, G]) extends `Type: ((x→x)→x→x)→x`[F]
case class `Type.Var: ((x→x)→x→x)→x`[T[_[_[_], _]]](lvl: Int, sym: AliasSym, impl: Option[`Type: ((x→x)→x→x)→x`[T]]) extends `Type: ((x→x)→x→x)→x`[T]

sealed trait `Type: x→x→x`[T[_, _]] extends Type
case class `Type.Lam: x→x→x`[F[_, _], A, B](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], codomain: `Type.App: x→x→x x x`[F, A, B]) extends `Type: x→x→x`[F]
case class `Type.Var: x→x→x`[T[_, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x→x→x`[T]]) extends `Type: x→x→x`[T]

sealed trait `Type: (x→x)→x→x`[T[_[_], _]] extends Type
case class `Type.Lam: (x→x)→x→x`[F[_[_], _], G[_], A](lvl: Int, domain1: `Type.Var: x→x`[G], domain2: `Type.Var: x`[A], codomain: `Type.App: (x→x)→x→x x→x x`[F, G, A]) extends `Type: (x→x)→x→x`[F]
case class `Type.Var: (x→x)→x→x`[T[_[_], _]](lvl: Int, sym: AliasSym, impl: Option[`Type: (x→x)→x→x`[T]]) extends `Type: (x→x)→x→x`[T]

// sealed trait `Type: x→x→x→x`[T[_, _, _]] extends Type
// case class `Type.Lam: x→x→x→x`[F[_, _, _], A, B, C](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], domain3: `Type.Var: x`[C], codomain: `Type.App: x→x→x→x x * x`[F, A, B, C]) extends Type
// case class `Type.Var: x→x→x→x`[T[_, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x→x→x→x`[T]]) extends Type

// sealed trait `Type: (x→x)→x→x→x`[T[_[_], _, _]] extends Type
// case class `Type.Lam: (x→x)→x→x→x`[F[_[_], _, _], G[_], A, B](lvl: Int, domain1: `Type.Var: x→x`[G], domain2: `Type.Var: x`[A], domain3: `Type.Var: x`[B], codomain: `Type.App: (x→x)→x→x→x (x→x) x x`[F, G, A, B]) extends Type
// case class `Type.Var: (x→x)→x→x→x`[T[_[_], _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: (x→x)→x→x→x`[T]]) extends Type

// sealed trait `Type: x→x→x→x→x`[T[_, _, _, _]] extends Type
// case class `Type.Lam: x→x→x→x→x`[F[_, _, _, _], A, B, C, D](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], domain3: `Type.Var: x`[C], domain4: `Type.Var: x`[D], codomain: `Type.App: x→x→x→x→x x * x x`[F, A, B, C, D]) extends Type
// case class `Type.Var: x→x→x→x→x`[T[_, _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x→x→x→x→x`[T]]) extends Type

// sealed trait `Type: (x→x)→x→x→x→x`[T[_[_], _, _, _]] extends Type
// case class `Type.Lam: (x→x)→x→x→x→x`[F[_[_], _, _, _], G[_], A, B, C](lvl: Int, domain1: `Type.Var: x→x`[G], domain2: `Type.Var: x`[A], domain3: `Type.Var: x`[B], domain4: `Type.Var: x`[C], codomain: `Type.App: (x→x)→x→x→x→x (x→x) x * x`[F, G, A, B, C]) extends Type
// case class `Type.Var: (x→x)→x→x→x→x`[T[_[_], _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: (x→x)→x→x→x→x`[T]]) extends Type
