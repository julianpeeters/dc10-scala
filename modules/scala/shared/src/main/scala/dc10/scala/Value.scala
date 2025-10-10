package dc10.scala

import cats.data.NonEmptyList

sealed trait Value:
  def lvl: Int

sealed trait `Value: x`[T] extends Value:
  def tpe: `Type: x`[T]
case class `Value.App.1: x`[A, B](lvl: Int, fun: `Value: x→x→x x x`[Function1, A, B], arg: `Value: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
// case class `Value.App.2: x`[A, B, C](lvl: Int, fun: `Value: x→x→x x x`[Function1, A, C], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
// case class `Value.App.3: x`[A, B, C, D](lvl: Int, fun: `Value: x`[(A, B, C) => D], arg1: `Value: x`[A], arg2: `Value: x`[B], arg3: `Value: x`[C], tpe: `Type: x`[D]) extends `Value: x`[D]
case class `Value.AppDot.0: x`[A, B](lvl: Int, fun: `Value: x`[A => B], arg1: `Value: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.AppDot.1: x`[A, B, C, D](lvl: Int, fun: `Value: x`[D], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.AppDotless: x`[A, B, C, D](lvl: Int, fun: `Value: x`[D], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.App.0: x→x`[G[_], A, B](lvl: Int, fun: `Value: x→x`[G], targ: `Type: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.App.0: (x→x)→x→x`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value: (x→x)→x→x`[G], targf: `Type: x→x`[H], targa: `Type: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
// case class `Value.App.0: (x→x)→x→x→x`[G[_[_], _, _], H[_], A, B, C](lvl: Int, fun: `Value: (x→x)→x→x→x`[G], targf: `Type: x→x`[H], targa: `Type: x`[A], targb: `Type: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.App.Vargs: x`[A, B](lvl: Int, fun: `Value: x`[List[A] => B], tpe: `Type: x`[B], vargs: `Value: x`[A]*) extends `Value: x`[B]
case class `Value.Def.0: x`[T](lvl: Int, sym: `DefSym.0`, tpe: `Type: x`[T], impl: Option[`Value: x`[T]]) extends `Value: x`[T]
case class `Value.Lit.Boolean: x`(lvl: Int, tpe: `Type: x`[Boolean], b: Boolean) extends `Value: x`[Boolean]
case class `Value.Lit.Int: x`(lvl: Int, tpe: `Type: x`[Int], i: Int) extends `Value: x`[Int]
case class `Value.Lit.Long: x`(lvl: Int, tpe: `Type: x`[Long], l: Long) extends `Value: x`[Long]
case class `Value.Lit.Float: x`(lvl: Int, tpe: `Type: x`[Float], f: Float) extends `Value: x`[Float]
case class `Value.Lit.Double: x`(lvl: Int, tpe: `Type: x`[Double], d: Double) extends `Value: x`[Double]
case class `Value.Lit.String: x`(lvl: Int, tpe: `Type: x`[String], s: String) extends `Value: x`[String]
case class `Value.Lit.Unit: x`(lvl: Int, tpe: `Type: x`[Unit], u: Unit) extends `Value: x`[Unit]
case class `Value.Obj: x`[T](lvl: Int, sym: ObjSym, tpe: `Type: x`[T], parent: Option[`Type: x`[T]], body: List[Statement]) extends `Value: x`[T]
case class `Value.Val: x`[T](lvl: Int, sym: ValSym, tpe: `Type: x`[T], impl: Option[`Value: x`[T]]) extends `Value: x`[T]

sealed trait `Value: x→x x`[T[_], A] extends Value:
  def tpe: `Type: x→x x`[T, A]
case class `Value.App.1: x→x x`[F[_], A, B](lvl: Int, fun: `Value: x→x→x x (x→x x)`[Function1, F, A, B], arg: `Value: x`[A], tpe: `Type: x→x x`[F, B]) extends `Value: x→x x`[F, B]
case class `Value.AppDot.1: x→x x`[F[_], A, B](lvl: Int, fun: `Value: x→x→x x (x→x x)`[Function1, F, A, B], targ: `Type: x→x`[F], arg: `Value: x`[A], tpe: `Type: x→x x`[F, B]) extends `Value: x→x x`[F, B]
case class `Value.AppForComp: x→x x`[G[_], A](lvl: Int, gens: NonEmptyList[Statement], ret: `Value: x`[A], tpe: `Type: x→x x`[G, A]) extends `Value: x→x x`[G, A]
case class `Value.Def.0: x→x x`[T[_], A](lvl: Int, sym: `DefSym.0`, tpe: `Type: x→x x`[T, A], impl: Option[`Value: x→x x`[T, A]]) extends `Value: x→x x`[T, A]
case class `Value.Val: x→x x`[T[_], A](lvl: Int, sym: ValSym, tpe: `Type: x→x x`[T, A], impl: Option[`Value: x→x x`[T, A]]) extends `Value: x→x x`[T, A]

sealed trait `Value: x→x ((x→x)→x→x x→x x)`[F[_], G[_[_], _], H[_], A] extends Value:
  def tpe: `Type: x→x ((x→x)→x→x x→x x)`[F, G, H, A]
case class `Value.AppDot.1: x→x ((x→x)→x→x x→x x)`[F[_], G[_[_], _], H[_], A, B](lvl: Int, fun: `Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, F, A, B], targ1: `Type: (x→x)→x→x x→x`[G, H], arg1: `Value: x`[A], tpe: `Type: x→x ((x→x)→x→x x→x x)`[F, G, H, B]) extends `Value: x→x ((x→x)→x→x x→x x)`[F, G, H, B]
case class `Value.Val: x→x ((x→x)→x→x x→x x)`[T[_], G[_[_], _], H[_], A](lvl: Int, sym: ValSym, tpe: `Type: x→x ((x→x)→x→x x→x x)`[T, G, H, A], impl: Option[`Value: x→x ((x→x)→x→x x→x x)`[T, G, H, A]]) extends `Value: x→x ((x→x)→x→x x→x x)`[T, G, H, A]

sealed trait `Value: x→x→x x x`[F[_, _], A, B] extends Value:
  def tpe: `Type: x→x→x x x`[F, A, B]
case class `Value.Def.1: x→x→x x x`[A, B](lvl: Int, sym: `DefSym.1`[A, B], tpe: `Type: x→x→x x x`[Function1, A, B], impl: Option[`Value: x`[B]]) extends `Value: x→x→x x x`[Function1, A, B]
case class `Value.Lam.1: x→x→x x x`[A, B](lvl: Int, a: `Value: x`[A], b: `Value: x`[B], tpe: `Type: x→x→x x x`[Function1, A, B]) extends `Value: x→x→x x x`[Function1, A, B]
case class `Value.Val: x→x→x x x`[T[_, _], A, B](lvl: Int, sym: ValSym, tpe: `Type: x→x→x x x`[T, A, B], impl: Option[`Value: x→x→x x x`[T, A, B]]) extends `Value: x→x→x x x`[T, A, B]

sealed trait `Value: x→x→x x (x→x x)`[F[_, _], G[_], A, B] extends Value:
  def tpe: `Type: x→x→x x (x→x x)`[F, G, A, B]
case class `Value.Def.1: x→x→x x (x→x x)`[G[_], A, B](lvl: Int, sym: `DefSym.1`[A, G[B]], tpe: `Type: x→x→x x (x→x x)`[Function1, G, A, B], impl: Option[`Value: x→x x`[G, B]]) extends `Value: x→x→x x (x→x x)`[Function1, G, A, B]

sealed trait `Value: x→x→x x ((x→x)→x→x x→x x)`[F[_, _], G[_[_], _], H[_], A, B] extends Value:
  def tpe: `Type: x→x→x x ((x→x)→x→x x→x x)`[F, G, H, A, B]
case class `Value.Def.1: x→x→x x ((x→x)→x→x x→x x)`[G[_[_], _], H[_], A, B](lvl: Int, sym: `DefSym.1`[A, G[H, B]], tpe: `Type: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B], impl: Option[`Value: (x→x)→x→x x→x x`[G, H, B]]) extends `Value: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B]
case class `Value.Val: x→x→x x ((x→x)→x→x x→x x)`[F[_, _], G[_[_], _], H[_], A, B](lvl: Int, sym: ValSym, tpe: `Type: x→x→x x ((x→x)→x→x x→x x)`[F, G, H, A, B], impl: Option[`Value: x→x→x x ((x→x)→x→x x→x x)`[F, G, H, A, B]]) extends `Value: x→x→x x ((x→x)→x→x x→x x)`[F, G, H, A, B]

sealed trait `Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[F[_, _], G[_[_], _], H[_], I[_], A, B] extends Value:
  def tpe: `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]
case class `Value.Def.1: x→x→x x (x→x ((x→x)→x→x x→x x))`[G[_[_], _], H[_], I[_], A, B](lvl: Int, sym: `DefSym.1`[A, I[G[H, B]]], tpe: `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, I, A, B], impl: Option[`Value: x→x ((x→x)→x→x x→x x)`[I, G, H, B]]) extends `Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, I, A, B]
case class `Value.Val: x→x→x x (x→x ((x→x)→x→x x→x x))`[F[_, _], G[_[_], _], H[_], I[_], A, B](lvl: Int, sym: ValSym, tpe: `Type: x→x→x x (x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B], impl: Option[`Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]]) extends `Value: x→x→x x (x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]

sealed trait `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends Value:
  def tpe: `Type: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F, G, H, I, A, B]
case class `Value.Def.0: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, sym: `DefSym.0`, tpe: `Type: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F, G, H, I, A, B], impl: Option[`Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F, G, H, I, A, B]]) extends `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[F, G, H, I, A, B]

sealed trait `Value: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends Value
// :
//   def tpe: `Type: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]
// case class `Value.Def.0: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, sym: `DefSym.0`, tpe: `Type: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B], impl: Option[`Value: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]]) extends `Value: x→x→x (x→x ((x→x)→x→x x→x x)) ((x→x)→x→x x→x ((x→x)→x→x x→x x))`[F, G, H, I, A, B]

sealed trait `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]] extends Value:
  def tpe: `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]
case class `Value.Def.1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, sym: `DefSym.1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[G, H, A], tpe: `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L], impl: Option[`Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]]) extends `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L]
case class `Value.Lam1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, a: `Value: (x→x)→x→x x→x x`[G, H, A], b: `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L], tpe: `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]) extends `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]
case class `Value.Val: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F[_, _], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, sym: ValSym, tpe: `Type: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L], impl: Option[`Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]]) extends `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[F, G, H, A, I, J, K, L]

sealed trait `Value: (x→x)→x→x x→x x`[T[_[_], _], F[_], A] extends Value:
  def tpe: `Type: (x→x)→x→x x→x x`[T, F, A]
// case class `Value.App.1: (x→x)→x→x x→x x`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B], arg: `Value: x→x x`[G, A], tpe: `Type: (x→x)→x→x x→x x`[I, H, B]) extends `Value: (x→x)→x→x x→x x`[I, H, B]
case class `Value.App.1: (x→x)→x→x x→x x`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B], arg: `Value: x`[A], tpe: `Type: (x→x)→x→x x→x x`[G, H, B]) extends `Value: (x→x)→x→x x→x x`[G, H, B]
case class `Value.AppDot.0: (x→x)→x→x x→x x`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B], arg1: `Value: x→x x`[G, A], tpe: `Type: (x→x)→x→x x→x x`[I, H, B]) extends `Value: (x→x)→x→x x→x x`[I, H, B]
case class `Value.AppDot.1: (x→x)→x→x x→x x`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B], targ1: `Type: (x→x)→x→x x→x`[G, H], arg1: `Value: x`[A], tpe: `Type: (x→x)→x→x x→x x`[G, H, B]) extends `Value: (x→x)→x→x x→x x`[G, H, B]
case class `Value.Val: (x→x)→x→x x→x x`[T[_[_], _], F[_], A](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x→x x→x x`[T, F, A], impl: Option[`Value: (x→x)→x→x x→x x`[T, F, A]]) extends `Value: (x→x)→x→x x→x x`[T, F, A]

sealed trait `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[T[_[_], _], F[_], G[_[_]], H[_]] extends Value:
  def tpe: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H]
case class `Value.App.1: (x→x)→x→x x→x ((x→x)→x x→x)`[G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, fun: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L], arg: `Value: (x→x)→x→x x→x x`[G, H, A], tpe: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]) extends `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
case class `Value.AppDot.1: (x→x)→x→x x→x ((x→x)→x x→x)`[G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](lvl: Int, fun: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L], arg0: `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[I, J, G, H, A] , arg: `Value: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, G, H, A, I, J, K, L], tpe: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]) extends `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
case class `Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[T[_[_], _], F[_], G[_[_]], H[_]](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H], impl: Option[`Value: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H]]) extends `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H]

sealed trait `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T[_[_], _], F[_], G[_[_], _], H[_], A] extends Value:
  def tpe: `Type: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T, F, G, H, A]
case class `Value.Val: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T[_[_], _], F[_], G[_[_], _], H[_], A](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T, F, G, H, A], impl: Option[`Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T, F, G, H, A]]) extends `Value: (x→x)→x→x x→x ((x→x)→x→x x→x x)`[T, F, G, H, A]


// sealed trait `Value.Expr: x→x→x→x x * x`[F[_, _, _], A, B, C] extends `Value: x`[F[A, B, C]]:
//   def tpe: `Type: x→x→x→x x * x`[F, A, B, C]
// case class `Value.Lam.2: x→x→x→x x * x`[A, B, C](lvl: Int, a1: `Value: x`[A], a2: `Value: x`[B], r: `Value: x`[C], tpe: `Type: x→x→x→x x * x`[Function2, A, B, C]) extends `Value.Expr: x→x→x→x x * x`[Function2, A, B, C]

// sealed trait `Value.Expr: x→x→x→x→x x * x x`[F[_, _, _, _], A, B, C, D] extends `Value: x`[F[A, B, C, D]]:
//   def tpe: `Type: x→x→x→x→x x * x x`[F, A, B, C, D]
// case class `Value.Lam.3: x→x→x→x→x x * x x`[A, B, C, D](lvl: Int, a1: `Value: x`[A], a2: `Value: x`[B], a3: `Value: x`[C], r: `Value: x`[D], tpe: `Type: x→x→x→x→x x * x x`[Function3, A, B, C, D]) extends `Value.Expr: x→x→x→x→x x * x x`[Function3, A, B, C, D]

sealed trait `Value: x→x`[T[_]] extends Value:
  def tpe: `Type: x→x`[T]
case class `Value.Val: x→x`[T[_]](lvl: Int, sym: ValSym, tpe: `Type: x→x`[T], impl: Option[`Value: x→x`[T]]) extends `Value: x→x`[T]

sealed trait `Value: (x→x)→x→x x→x`[T[_[_], _], F[_]] extends Value:
  def tpe: `Type: (x→x)→x→x x→x`[T, F]
case class `Value.App.1: (x→x)→x→x x→x`[G[_[_], _], H[_], A](lvl: Int, fun: `Value: x→x→x x ((x→x)→x→x x→x)`[Function1, G, H, A], targ: `Type: x→x`[H], arg: `Value: x`[A], tpe: `Type: (x→x)→x→x x→x`[G, H]) extends `Value: (x→x)→x→x x→x`[G, H]
case class `Value.AppDot.0: (x→x)→x→x x→x`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B], targ: `Type: x→x`[H], arg1: `Value: x→x x`[G, A], tpe: `Type: (x→x)→x→x x→x`[I, H]) extends `Value: (x→x)→x→x x→x`[I, H]
case class `Value.Val: (x→x)→x→x x→x`[T[_[_], _], F[_], A](lvl: Int, sym: ValSym, targ: `Type: x→x`[F], tpe: `Type: (x→x)→x→x x→x`[T, F], impl: Option[`Value: (x→x)→x→x x→x`[T, F]]) extends `Value: (x→x)→x→x x→x`[T, F]

sealed trait `Value: x→x→x x ((x→x)→x→x x→x)`[F[_, _], G[_[_], _], H[_], A] extends Value:
  def tpe: `Type: x→x→x x ((x→x)→x→x x→x)`[F, G, H, A]
// case class `Value.Def.1: x→x→x x ((x→x)→x→x x→x)`[G[_[_], _], H[_], A](lvl: Int, sym: `DefSym.1`[A, [B] =>> G[H, B]], tpe: `Type: x→x→x x ((x→x)→x→x x→x)`[Function1, G, H, A], impl: Option[`Value: (x→x)→x→x x→x`[G, H]]) extends `Value: x→x→x x ((x→x)→x→x x→x)`[Function1, G, H, A]
case class `Value.Val: x→x→x x ((x→x)→x→x x→x)`[F[_, _], G[_[_], _], H[_], A](lvl: Int, sym: ValSym, tpe: `Type: x→x→x x ((x→x)→x→x x→x)`[F, G, H, A], impl: Option[`Value: x→x→x x ((x→x)→x→x x→x)`[F, G, H, A]]) extends `Value: x→x→x x ((x→x)→x→x x→x)`[F, G, H, A]

sealed trait `Value: (x→x)→x`[T[_[_]]] extends Value:
  def tpe: `Type: (x→x)→x`[T]
case class `Value.Val: (x→x)→x`[T[_[_]]](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x`[T], impl: Option[`Value: (x→x)→x`[T]]) extends `Value: (x→x)→x`[T]

sealed trait `Value: (x→x)→x x→x`[T[_[_]], F[_]] extends Value:
  def tpe: `Type: (x→x)→x x→x`[T, F]
case class `Value.Val: (x→x)→x x→x`[T[_[_]], F[_]](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x x→x`[T, F], impl: Option[`Value: (x→x)→x x→x`[T, F]]) extends `Value: (x→x)→x x→x`[T, F]

sealed trait `Value: x→x→x`[T[_, _]] extends Value:
  def tpe: `Type: x→x→x`[T]
case class `Value.Val: x→x→x`[T[_, _]](lvl: Int, sym: ValSym, tpe: `Type: x→x→x`[T], impl: Option[`Value: x→x→x`[T]]) extends `Value: x→x→x`[T]

sealed trait `Value: (x→x)→x→x`[T[_[_], _]] extends Value:
  def tpe: `Type: (x→x)→x→x`[T]
case class `Value.Val: (x→x)→x→x`[T[_[_], _]](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x→x`[T], impl: Option[`Value: (x→x)→x→x`[T]]) extends `Value: (x→x)→x→x`[T]

// sealed trait `Value.Expr: (x→x)→x→x→x`[T[_[_], _, _]] extends `Value: (x→x)→x→x→x`[T]:
//   def tpe: `Type: (x→x)→x→x→x`[T]
// case class `Value.Val: (x→x)→x→x→x`[T[_[_], _, _]](lvl: Int, sym: ValSym, tpe: `Type: (x→x)→x→x→x`[T], impl: Option[`Value.Expr: (x→x)→x→x→x`[T]]) extends `Value.Expr: (x→x)→x→x→x`[T]