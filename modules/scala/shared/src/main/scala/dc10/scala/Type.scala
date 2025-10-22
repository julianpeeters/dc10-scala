package dc10.scala

sealed trait Type:
  def lvl: Int

sealed trait `Type: x`[+T] extends Type
case class `Type.Lit: x`[T](lvl: Int, nme: String) extends `Type: x`[T]
case class `Type.Var: x`[T](lvl: Int, sym: AliasSym, impl: Option[`Type: x`[T]]) extends `Type: x`[T]

sealed trait `Type: x_x x`[T[_], A] extends Type:
  def targ1: `Type: x`[A]
case class `Type.App: x_x x`[T[_], A](lvl: Int, tfun: `Type: x_x`[T], targ1: `Type: x`[A]) extends `Type: x_x x`[T, A]
case class `Type.Var: x_x x`[T[_], A](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], impl: Option[`Type: x`[T[A]]]) extends `Type: x_x x`[T, A]

sealed trait `Type: lx_xl_x x_x`[T[_[_]], F[_]] extends Type:
  def targ1: `Type: x_x`[F]
case class `Type.App: lx_xl_x x_x`[T[_[_]], F[_]](lvl: Int, tfun: `Type: lx_xl_x`[T], targ1: `Type: x_x`[F]) extends `Type: lx_xl_x x_x`[T, F]
case class `Type.Var: lx_xl_x x_x`[T[_[_]], F[_]](lvl: Int, sym: AliasSym, targ1: `Type: x_x`[F], impl: Option[`Type: lx_xl_x x_x`[T, F]]) extends `Type: lx_xl_x x_x`[T, F]

sealed trait `Type: llx_xl_xl_x lx_xl_x`[T[_[_[_]]], F[_[_]]] extends Type:
  def targ1: `Type: lx_xl_x`[F]
case class `Type.App: llx_xl_xl_x lx_xl_x`[T[_[_[_]]], F[_[_]]](lvl: Int, tfun: `Type: llx_xl_xl_x`[T], targ1: `Type: lx_xl_x`[F]) extends `Type: llx_xl_xl_x lx_xl_x`[T, F]
case class `Type.Var: llx_xl_xl_x lx_xl_x`[T[_[_[_]]], F[_[_]]](lvl: Int, sym: AliasSym, targ1: `Type: lx_xl_x`[F], impl: Option[`Type: llx_xl_xl_x lx_xl_x`[T, F]]) extends `Type: llx_xl_xl_x lx_xl_x`[T, F]

sealed trait `Type: llx_x_xl_xl_x x_x_x`[T[_[_, _]], F[_, _]] extends Type:
  def targ1: `Type: x_x_x`[F]
case class `Type.App: lx_x_xl_x x_x_x`[T[_[_, _]], F[_, _]](lvl: Int, tfun: `Type: lx_x_xl_x`[T], targ1: `Type: x_x_x`[F]) extends `Type: llx_x_xl_xl_x x_x_x`[T, F]
case class `Type.Var: lx_x_xl_x x_x_x`[T[_[_, _]], F[_, _]](lvl: Int, sym: AliasSym, targ1: `Type: x_x_x`[F], impl: Option[`Type: llx_x_xl_xl_x x_x_x`[T, F]]) extends `Type: llx_x_xl_xl_x x_x_x`[T, F]

sealed trait `Type: llx_xl_x_xl_x lx_xl_x_x`[T[_[_[_], _]], F[_[_], _]] extends Type:
  def targ1: `Type: lx_xl_x_x`[F]
case class `Type.App: llx_xl_x_xl_x lx_xl_x_x`[T[_[_[_], _]], F[_[_], _]](lvl: Int, tfun: `Type: llx_xl_x_xl_x`[T], targ1: `Type: lx_xl_x_x`[F]) extends `Type: llx_xl_x_xl_x lx_xl_x_x`[T, F]
case class `Type.Var: llx_xl_x_xl_x lx_xl_x_x`[T[_[_[_], _]], F[_[_], _]](lvl: Int, sym: AliasSym, targ1: `Type: lx_xl_x_x`[F], impl: Option[`Type: llx_xl_x_xl_x lx_xl_x_x`[T, F]]) extends `Type: llx_xl_x_xl_x lx_xl_x_x`[T, F]

sealed trait `Type: x_x_x x x`[T[_,_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: x`[B]
case class `Type.App: x_x_x x x`[T[_,_], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B]) extends `Type: x_x_x x x`[T, A, B]
case class `Type.AppInfix: x_x_x x x`[T[_,_], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B]) extends `Type: x_x_x x x`[T, A, B]
case class `Type.Var: x_x_x x x`[T[_,_], A, B](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B], impl: Option[`Type: x_x_x x x`[T, A, B]]) extends `Type: x_x_x x x`[T, A, B]

sealed trait `Type: x_x_x x lx_x xl`[F[_, _], G[_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: x_x x`[G, B]
case class `Type.AppInfix: x_x_x x lx_x xl`[T[_,_], G[_], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x_x x`[G, B]) extends `Type: x_x_x x lx_x xl`[T, G, A, B]

sealed trait `Type: x_x_x x llx_xl_x_x x_x xl`[F[_, _], G[_[_], _], H[_], A, B] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: lx_xl_x_x x_x x`[G, H, B]
case class `Type.AppInfix: x_x_x x llx_xl_x_x x_x xl`[T[_,_], G[_[_], _], H[_], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: lx_xl_x_x x_x x`[G, H, B]) extends `Type: x_x_x x llx_xl_x_x x_x xl`[T, G, H, A, B]

sealed trait `Type: x_x_x x lx_x llx_xl_x_x x_x xll`[F[_, _], G[_[_], _], H[_], I[_], A, B] extends Type:
  def targ2: `Type: x_x llx_xl_x_x x_x xl`[I, G, H, B]
case class `Type.AppInfix: x_x_x x lx_x llx_xl_x_x x_x xll`[T[_,_], G[_[_], _], H[_], I[_], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x_x llx_xl_x_x x_x xl`[I, G, H, B]) extends `Type: x_x_x x lx_x llx_xl_x_x x_x xll`[T, G, H, I, A, B]

sealed trait `Type: x_x_x lx_x xl llx_xl_x_x x_x xl`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends Type:
  def targ1: `Type: x_x x`[G, A]
  def targ2: `Type: lx_xl_x_x x_x x`[I, H, B]
case class `Type.AppInfix: x_x_x lx_x xl llx_xl_x_x x_x xl`[T[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x_x x`[G, A], targ2: `Type: lx_xl_x_x x_x x`[I, H, B]) extends `Type: x_x_x lx_x xl llx_xl_x_x x_x xl`[T, G, H, I, A, B]

sealed trait `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]] extends Type:
  def targ1: `Type: lx_xl_x_x x_x x`[G, H, A]
  def targ2: `Type: lx_xl_x_x x_x llx_xl_x x_xl`[I, J, K, L]
case class `Type.AppInfix: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, tfun: `Type: x_x_x`[F], targ1: `Type: lx_xl_x_x x_x x`[G, H, A], targ2: `Type: lx_xl_x_x x_x llx_xl_x x_xl`[I, J, K, L]) extends `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F, G, H, A, I, J, K, L]

sealed trait `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B] extends Type:
  def targ1: `Type: lx_xl_x_x x_x x`[G, H, A]
  def targ2: `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[I, J, K, L, B]
case class `Type.AppInfix: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B](lvl: Int, tfun: `Type: x_x_x`[F], targ1: `Type: lx_xl_x_x x_x x`[G, H, A], targ2: `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[I, J, K, L, B]) extends `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`[F, G, H, A, I, J, K, L, B]

sealed trait `Type: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`[T[_, _], F[_], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B] extends Type:
  def targ1: `Type: x_x llx_xl_x_x x_x xl`[F, G, H, A]
  def targ2: `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[I, J, K, L, B]
case class `Type.AppInfix: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`[T[_, _], F[_], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x_x llx_xl_x_x x_x xl`[F, G, H, A], targ2: `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[I, J, K, L, B]) extends `Type: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`[T, F, G, H, A, I, J, K, L, B]

sealed trait `Type: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`[T[_, _], F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_], M[_[_], _], N[_], O[_[_]], P[_]] extends Type:
  def targ1: `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F, G, H, A, I, J, K, L]
  def targ2: `Type: lx_xl_x_x x_x llx_xl_x x_xl`[M, N, O, P]
case class `Type.AppInfix: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`[T[_, _], F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_], M[_[_], _], N[_], O[_[_]], P[_]](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F, G, H, A, I, J, K, L], targ2: `Type: lx_xl_x_x x_x llx_xl_x x_xl`[M, N, O, P]) extends `Type: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`[T, F, G, H, A, I, J, K, L, M, N, O, P]

sealed trait `Type: lx_xl_x_x x_x x`[T[_[_], _], F[_], A] extends Type:
  def targ1: `Type: x_x`[F]
  def targ2: `Type: x`[A]
case class `Type.App: lx_xl_x_x x_x x`[T[_[_], _], F[_], A](lvl: Int, tfun: `Type: lx_xl_x_x`[T], targ1: `Type: x_x`[F], targ2: `Type: x`[A]) extends `Type: lx_xl_x_x x_x x`[T, F, A]
case class `Type.Var: lx_xl_x_x x_x x`[T[_[_],_], F[_], A](lvl: Int, sym: AliasSym, targ1: `Type: x_x`[F], targ2: `Type: x`[A], impl: Option[`Type: x`[T[F, A]]]) extends `Type: lx_xl_x_x x_x x`[T, F, A]

sealed trait `Type: lx_xl_x_x x_x llx_xl_x x_xl`[T[_[_], _], F[_], G[_[_]], H[_]] extends Type:
  def targ1: `Type: x_x`[F]
  def targ2: `Type: lx_xl_x x_x`[G, H]
case class `Type.App: lx_xl_x_x x_x llx_xl_x x_xl`[T[_[_], _], F[_], G[_[_]], H[_]](lvl: Int, tfun: `Type: lx_xl_x_x`[T], targ1: `Type: x_x`[F], targ2: `Type: lx_xl_x x_x`[G, H]) extends `Type: lx_xl_x_x x_x llx_xl_x x_xl`[T, F, G, H]

sealed trait `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[T[_[_], _], F[_], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: x_x`[F]
  def targ2: `Type: lx_xl_x_x x_x x`[G, H, A]
case class `Type.App: lx_xl_x_x x_x llx_xl_x_x x_x xl`[T[_[_], _], F[_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: lx_xl_x_x`[T], targ1: `Type: x_x`[F], targ2: `Type: lx_xl_x_x x_x x`[G, H, A]) extends `Type: lx_xl_x_x x_x llx_xl_x_x x_x xl`[T, F, G, H, A]

sealed trait `Type: x_x llx_xl_x_x x_x xl`[F[_], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: lx_xl_x_x x_x x`[G, H, A]
case class `Type.App: x_x llx_xl_x_x x_x xl`[F[_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: x_x`[F], targ1: `Type: lx_xl_x_x x_x x`[G, H, A]) extends `Type: x_x llx_xl_x_x x_x xl`[F, G, H, A]

// sealed trait `Type: x_x_x_x x * x`[T[_,_,_], A, B, C] extends Type
//   def targ1: `Type: x`[A]
//   def targ2: `Type: x`[B]
//   def targ3: `Type: x`[C]
// case class `Type.App: x_x_x_x x * x`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type: x_x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C]) extends `Type: x_x_x_x x * x`[T, A, B, C]
// case class `Type.AppInfix: x_x_x_x x * x`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type: x_x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C]) extends `Type: x_x_x_x x * x`[T, A, B, C]
// case class `Type.Var: x_x_x_x x * x`[T[_,_,_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B],  targ3: `Type: x`[C], impl: Option[`Type: x_x_x_x x * x`[T, A, B, C]]) extends `Type: x_x_x_x x * x`[T, A, B, C]

// sealed trait `Type: lx_xl_x_x_x lx_xl x x`[T[_[_],_,_], F[_], A, B] extends Type
//   def targ1: `Type: x_x`[F]
//   def targ2: `Type: x`[A]
//   def targ3: `Type: x`[B]
// case class `Type.App: lx_xl_x_x_x lx_xl x x`[T[_[_], _, _], F[_], A, B](lvl: Int, tfun: `Type: lx_xl_x_x_x`[T], targ1: `Type: x_x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B]) extends `Type: lx_xl_x_x_x lx_xl x x`[T, F, A, B]
// case class `Type.Var: lx_xl_x_x_x lx_xl x x`[T[_[_], _,_], F[_], A, B](lvl: Int, sym: AliasSym, targ1: `Type: x_x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B], impl: Option[`Type: lx_xl_x_x_x lx_xl x x`[T, F, A, B]]) extends `Type: lx_xl_x_x_x lx_xl x x`[T, F, A, B]

// sealed trait `Type: x_x_x_x_x x * x x`[T[_,_,_,_], A, B, C, D] extends Type
//   def targ1: `Type: x`[A]
//   def targ2: `Type: x`[B]
//   def targ3: `Type: x`[C]
//   def targ4: `Type: x`[D]
// case class `Type.App: x_x_x_x_x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type: x_x_x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C], targ4: `Type: x`[D]) extends `Type: x_x_x_x_x x * x x`[T, A, B, C, D]
// case class `Type.AppInfix: x_x_x_x_x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type: x_x_x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: x`[B], targ3: `Type: x`[C], targ4: `Type: x`[D]) extends `Type: x_x_x_x_x x * x x`[T, A, B, C, D]
// case class `Type.Var: x_x_x_x_x x * x x`[T[_,_,_,_], A, B, C, D](lvl: Int, sym: AliasSym, targ1: `Type: x`[A], targ2: `Type: x`[B],  targ3: `Type: x`[C],  targ4: `Type: x`[D], impl: Option[`Type: x_x_x_x_x x * x x`[T, A, B, C, D]]) extends `Type: x_x_x_x_x x * x x`[T, A, B, C, D]

// sealed trait `Type: lx_xl_x_x_x_x lx_xl x * x`[T[_[_],_,_,_], F[_], A, B, C] extends Type
//   def targ1: `Type: x_x`[F]
//   def targ2: `Type: x`[A]
//   def targ3: `Type: x`[B]
//   def targ4: `Type: x`[C]
// case class `Type.App: lx_xl_x_x_x_x lx_xl x * x`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, tfun: `Type: lx_xl_x_x_x_x`[T], targ1: `Type: x_x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B], targ4: `Type: x`[C]) extends `Type: lx_xl_x_x_x_x lx_xl x * x`[T, F, A, B, C]
// case class `Type.Var: lx_xl_x_x_x_x lx_xl x * x`[T[_[_],_,_,_], F[_], A, B, C](lvl: Int, sym: AliasSym, targ1: `Type: x_x`[F], targ2: `Type: x`[A], targ3: `Type: x`[B],  targ4: `Type: x`[C], impl: Option[`Type: lx_xl_x_x_x_x lx_xl x * x`[T, F, A, B, C]]) extends `Type: lx_xl_x_x_x_x lx_xl x * x`[T, F, A, B, C]

sealed trait `Type: x_x`[T[_]] extends Type
case class `Type.Lam: x_x`[F[_], A](lvl: Int, domain: `Type.Var: x`[A], codomain: `Type.App: x_x x`[F, A]) extends `Type: x_x`[F]
// case class `Type.Var: x_x`[T[_]](lvl: Int, sym: AliasSym, impl: Option[`Type: x_x`[[A] =>> T[A]]], ctors: () => List[Value]) extends `Type: x_x`[T]
case class `Type.Var: x_x`[T[_]](lvl: Int, sym: AliasSym, impl: Option[`Type: x_x`[[A] =>> T[A]]]) extends `Type: x_x`[T]

sealed trait `Type: lx_xl_x_x x_x`[T[_[_], _], F[_]] extends Type:
  def tfun: `Type: lx_xl_x_x`[T]
  def targ1: `Type: x_x`[F]
case class `Type.App: lx_xl_x_x x_x`[T[_[_], _], F[_]](lvl: Int, tfun: `Type: lx_xl_x_x`[T], targ1: `Type: x_x`[F]) extends `Type: lx_xl_x_x x_x`[T, F]
case class `Type.Var: lx_xl_x_x x_x`[T[_[_],_], F[_]](lvl: Int, sym: AliasSym, tfun: `Type: lx_xl_x_x`[T], targ1: `Type: x_x`[F], impl: Option[`Type: x_x`[[A] =>> T[F, A]]]) extends `Type: lx_xl_x_x x_x`[T, F]

sealed trait `Type: x_x_x x llx_xl_x_x x_xl`[F[_, _], G[_[_], _], H[_], A] extends Type:
  def targ1: `Type: x`[A]
  def targ2: `Type: lx_xl_x_x x_x`[G, H]
case class `Type.AppInfix: x_x_x x llx_xl_x_x x_xl`[T[_,_], G[_[_], _], H[_], A](lvl: Int, tfun: `Type: x_x_x`[T], targ1: `Type: x`[A], targ2: `Type: lx_xl_x_x x_x`[G, H]) extends `Type: x_x_x x llx_xl_x_x x_xl`[T, G, H, A]

sealed trait `Type: lx_xl_x`[T[_[_]]] extends Type
case class `Type.Lam: lx_xl_x`[F[_[_]], G[_]](lvl: Int, domain: `Type.Var: x_x`[G], codomain: `Type.App: lx_xl_x x_x`[F, G]) extends `Type: lx_xl_x`[F]
case class `Type.Var: lx_xl_x`[T[_[_]]](lvl: Int, sym: AliasSym, impl: Option[`Type: lx_xl_x`[T]]) extends `Type: lx_xl_x`[T]

sealed trait `Type: llx_xl_xl_x`[T[_[_[_]]]] extends Type
case class `Type.Lam: llx_xl_xl_x`[F[_[_[_]]], G[_[_]]](lvl: Int, domain: `Type.Var: lx_xl_x`[G], codomain: `Type.App: llx_xl_xl_x lx_xl_x`[F, G]) extends `Type: llx_xl_xl_x`[F]
case class `Type.Var: llx_xl_xl_x`[T[_[_[_]]]](lvl: Int, sym: AliasSym, impl: Option[`Type: llx_xl_xl_x`[T]]) extends `Type: llx_xl_xl_x`[T]

sealed trait `Type: lx_x_xl_x`[T[_[_, _]]] extends Type
case class `Type.Lam: lx_x_xl_x`[F[_[_, _]], G[_, _]](lvl: Int, domain: `Type.Var: x_x_x`[G], codomain: `Type.App: lx_x_xl_x x_x_x`[F, G]) extends `Type: lx_x_xl_x`[F]
case class `Type.Var: lx_x_xl_x`[T[_[_, _]]](lvl: Int, sym: AliasSym, impl: Option[`Type: lx_x_xl_x`[T]]) extends `Type: lx_x_xl_x`[T]

sealed trait `Type: llx_xl_x_xl_x`[T[_[_[_], _]]] extends Type
case class `Type.Lam: llx_xl_x_xl_x`[F[_[_[_], _]], G[_[_], _]](lvl: Int, domain: `Type.Var: lx_xl_x_x`[G], codomain: `Type.App: llx_xl_x_xl_x lx_xl_x_x`[F, G]) extends `Type: llx_xl_x_xl_x`[F]
case class `Type.Var: llx_xl_x_xl_x`[T[_[_[_], _]]](lvl: Int, sym: AliasSym, impl: Option[`Type: llx_xl_x_xl_x`[T]]) extends `Type: llx_xl_x_xl_x`[T]

sealed trait `Type: x_x_x`[T[_, _]] extends Type
case class `Type.Lam: x_x_x`[F[_, _], A, B](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], codomain: `Type.App: x_x_x x x`[F, A, B]) extends `Type: x_x_x`[F]
case class `Type.Var: x_x_x`[T[_, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x_x_x`[T]]) extends `Type: x_x_x`[T]

sealed trait `Type: lx_xl_x_x`[T[_[_], _]] extends Type
case class `Type.Lam: lx_xl_x_x`[F[_[_], _], G[_], A](lvl: Int, domain1: `Type.Var: x_x`[G], domain2: `Type.Var: x`[A], codomain: `Type.App: lx_xl_x_x x_x x`[F, G, A]) extends `Type: lx_xl_x_x`[F]
case class `Type.Var: lx_xl_x_x`[T[_[_], _]](lvl: Int, sym: AliasSym, impl: Option[`Type: lx_xl_x_x`[T]]) extends `Type: lx_xl_x_x`[T]

// sealed trait `Type: x_x_x_x`[T[_, _, _]] extends Type
// case class `Type.Lam: x_x_x_x`[F[_, _, _], A, B, C](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], domain3: `Type.Var: x`[C], codomain: `Type.App: x_x_x_x x * x`[F, A, B, C]) extends Type
// case class `Type.Var: x_x_x_x`[T[_, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x_x_x_x`[T]]) extends Type

// sealed trait `Type: lx_xl_x_x_x`[T[_[_], _, _]] extends Type
// case class `Type.Lam: lx_xl_x_x_x`[F[_[_], _, _], G[_], A, B](lvl: Int, domain1: `Type.Var: x_x`[G], domain2: `Type.Var: x`[A], domain3: `Type.Var: x`[B], codomain: `Type.App: lx_xl_x_x_x lx_xl x x`[F, G, A, B]) extends Type
// case class `Type.Var: lx_xl_x_x_x`[T[_[_], _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: lx_xl_x_x_x`[T]]) extends Type

// sealed trait `Type: x_x_x_x_x`[T[_, _, _, _]] extends Type
// case class `Type.Lam: x_x_x_x_x`[F[_, _, _, _], A, B, C, D](lvl: Int, domain1: `Type.Var: x`[A], domain2: `Type.Var: x`[B], domain3: `Type.Var: x`[C], domain4: `Type.Var: x`[D], codomain: `Type.App: x_x_x_x_x x * x x`[F, A, B, C, D]) extends Type
// case class `Type.Var: x_x_x_x_x`[T[_, _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: x_x_x_x_x`[T]]) extends Type

// sealed trait `Type: lx_xl_x_x_x_x`[T[_[_], _, _, _]] extends Type
// case class `Type.Lam: lx_xl_x_x_x_x`[F[_[_], _, _, _], G[_], A, B, C](lvl: Int, domain1: `Type.Var: x_x`[G], domain2: `Type.Var: x`[A], domain3: `Type.Var: x`[B], domain4: `Type.Var: x`[C], codomain: `Type.App: lx_xl_x_x_x_x lx_xl x * x`[F, G, A, B, C]) extends Type
// case class `Type.Var: lx_xl_x_x_x_x`[T[_[_], _, _, _]](lvl: Int, sym: AliasSym, impl: Option[`Type: lx_xl_x_x_x_x`[T]]) extends Type
