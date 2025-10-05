package dc10.scala

import cats.data.NonEmptyList

sealed trait Value:
  def lvl: Int

sealed trait `Value: *`[+T] extends Value

sealed trait `Value: *→*`[T[_]] extends Value:
  def tpe: `Type: *→*`[T]

sealed trait `Value: (*→*)→*`[T[_[_]]] extends Value:
  def tpe: `Type: (*→*)→*`[T]
  
sealed trait `Value: *→*→*`[T[_, _]] extends Value:
  def tpe: `Type: *→*→*`[T]

sealed trait `Value: (*→*)→*→*`[T[_[_], _]] extends Value:
  def tpe: `Type: (*→*)→*→*`[T]

sealed trait `Value: *→*→*→*`[T[_, _, _]] extends Value:
  def tpe: `Type: *→*→*→*`[T]

sealed trait `Value: (*→*)→*→*→*`[T[_[_], _, _]] extends Value:
  def tpe: `Type: (*→*)→*→*→*`[T]

sealed trait `Value.Expr: *`[T] extends `Value: *`[T]:
  def tpe: `Type.Expr: *`[T]
case class `Value.App.1: *`[A, B](lvl: Int, fun: `Value: *`[A => B], arg: `Value: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.2: *`[A, B, C](lvl: Int, fun: `Value: *`[(A, B) => C], arg1: `Value: *`[A], arg2: `Value: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.App.3: *`[A, B, C, D](lvl: Int, fun: `Value: *`[(A, B, C) => D], arg1: `Value: *`[A], arg2: `Value: *`[B], arg3: `Value: *`[C], tpe: `Type.Expr: *`[D]) extends `Value.Expr: *`[D]
case class `Value.AppDot.0: *`[A, B](lvl: Int, fun: `Value: *`[A => B], arg1: `Value: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.AppDot.1: *`[A, B, C, D](lvl: Int, fun: `Value: *`[D], arg1: `Value: *`[A], arg2: `Value: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.AppDotless: *`[A, B, C, D](lvl: Int, fun: `Value: *`[D], arg1: `Value: *`[A], arg2: `Value: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.App.0: *→*`[G[_], A, B](lvl: Int, fun: `Value: *→*`[G], targ: `Type: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.0: (*→*)→*→*`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value: (*→*)→*→*`[G], targf: `Type: *→*`[H], targa: `Type: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.0: (*→*)→*→*→*`[G[_[_], _, _], H[_], A, B, C](lvl: Int, fun: `Value: (*→*)→*→*→*`[G], targf: `Type: *→*`[H], targa: `Type: *`[A], targb: `Type: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.App.Vargs: *`[A, B](lvl: Int, fun: `Value: *`[List[A] => B], tpe: `Type.Expr: *`[B], vargs: `Value: *`[A]*) extends `Value.Expr: *`[B]
case class `Value.Def.0: *`[T](lvl: Int, sym: `DefSym.0`, tpe: `Type.Expr: *`[T], impl: Option[`Value: *`[T]]) extends `Value.Expr: *`[T]
case class `Value.Lit.Boolean: *`(lvl: Int, tpe: `Type.Expr: *`[Boolean], b: Boolean) extends `Value.Expr: *`[Boolean]
case class `Value.Lit.Int: *`(lvl: Int, tpe: `Type.Expr: *`[Int], i: Int) extends `Value.Expr: *`[Int]
case class `Value.Lit.Long: *`(lvl: Int, tpe: `Type.Expr: *`[Long], l: Long) extends `Value.Expr: *`[Long]
case class `Value.Lit.Float: *`(lvl: Int, tpe: `Type.Expr: *`[Float], f: Float) extends `Value.Expr: *`[Float]
case class `Value.Lit.Double: *`(lvl: Int, tpe: `Type.Expr: *`[Double], d: Double) extends `Value.Expr: *`[Double]
case class `Value.Lit.String: *`(lvl: Int, tpe: `Type.Expr: *`[String], s: String) extends `Value.Expr: *`[String]
case class `Value.Lit.Unit: *`(lvl: Int, tpe: `Type.Expr: *`[Unit], u: Unit) extends `Value.Expr: *`[Unit]
case class `Value.Obj: *`[T](lvl: Int, sym: ObjSym, tpe: `Type.Expr: *`[T], parent: Option[`Type: *`[T]], body: List[Statement]) extends `Value.Expr: *`[T]
case class `Value.Val: *`[T](lvl: Int, sym: ValSym, tpe: `Type.Expr: *`[T], impl: Option[`Value: *`[T]]) extends `Value.Expr: *`[T]

sealed trait `Value.Expr: *→* *`[T[_], A] extends `Value: *`[T[A]]:
  def tpe: `Type.Expr: *→* *`[T, A]
case class `Value.App.1: *→* *`[F[_], A, B](lvl: Int, fun: `Value.Expr: *→*→* * (*→* *)`[Function1, F, A, B], arg: `Value: *`[A], tpe: `Type.Expr: *→* *`[F, B]) extends `Value.Expr: *→* *`[F, B]
case class `Value.AppDot.1: *→* *`[F[_], A, B](lvl: Int, fun: `Value.Expr: *→*→* * (*→* *)`[Function1, F, A, B], targ: `Type.Expr: *→*`[F], arg: `Value: *`[A], tpe: `Type.Expr: *→* *`[F, B]) extends `Value.Expr: *→* *`[F, B]
case class `Value.AppForComp: *→* *`[G[_], A](lvl: Int, gens: NonEmptyList[Statement], ret: `Value: *`[A], tpe: `Type.Expr: *→* *`[G, A]) extends `Value.Expr: *→* *`[G, A]
case class `Value.Def.0: *→* *`[T[_], A](lvl: Int, sym: `DefSym.0`, tpe: `Type.Expr: *→* *`[T, A], impl: Option[`Value.Expr: *→* *`[T, A]]) extends `Value.Expr: *→* *`[T, A]
case class `Value.Val: *→* *`[T[_], A](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→* *`[T, A], impl: Option[`Value.Expr: *→* *`[T, A]]) extends `Value.Expr: *→* *`[T, A]

sealed trait `Value.Expr: *→* ((*→*)→*→* *→* *)`[F[_], G[_[_], _], H[_], A] extends `Value: *`[F[G[H, A]]]:
  def tpe: `Type.Expr: *→* ((*→*)→*→* *→* *)`[F, G, H, A]
case class `Value.AppDot.1: *→* ((*→*)→*→* *→* *)`[F[_], G[_[_], _], H[_], A, B](lvl: Int, fun: `Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, F, A, B], targ1: `Type.Expr: (*→*)→*→* *→*`[G, H], arg1: `Value.Expr: *`[A], tpe: `Type.Expr: *→* ((*→*)→*→* *→* *)`[F, G, H, B]) extends `Value.Expr: *→* ((*→*)→*→* *→* *)`[F, G, H, B]
case class `Value.Val: *→* ((*→*)→*→* *→* *)`[T[_], G[_[_], _], H[_], A](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→* ((*→*)→*→* *→* *)`[T, G, H, A], impl: Option[`Value.Expr: *→* ((*→*)→*→* *→* *)`[T, G, H, A]]) extends `Value.Expr: *→* ((*→*)→*→* *→* *)`[T, G, H, A]

sealed trait `Value.Expr: *→*→* * *`[F[_, _], A, B] extends `Value: *`[F[A, B]]:
  def tpe: `Type.Expr: *→*→* * *`[F, A, B]
case class `Value.Def.1: *→*→* * *`[A, B](lvl: Int, sym: `DefSym.1`[A, B], tpe: `Type.Expr: *→*→* * *`[Function1, A, B], impl: Option[`Value: *`[B]]) extends `Value.Expr: *→*→* * *`[Function1, A, B]
case class `Value.Lam.1: *→*→* * *`[A, B](lvl: Int, a: `Value: *`[A], b: `Value: *`[B], tpe: `Type.Expr: *→*→* * *`[Function1, A, B]) extends `Value.Expr: *→*→* * *`[Function1, A, B]
case class `Value.Val: *→*→* * *`[T[_, _], A, B](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*→* * *`[T, A, B], impl: Option[`Value.Expr: *→*→* * *`[T, A, B]]) extends `Value.Expr: *→*→* * *`[T, A, B]

sealed trait `Value.Expr: *→*→* * (*→* *)`[F[_, _], G[_], A, B] extends `Value: *`[F[A, G[B]]]:
  def tpe: `Type.Expr: *→*→* * (*→* *)`[F, G, A, B]
case class `Value.Def.1: *→*→* * (*→* *)`[G[_], A, B](lvl: Int, sym: `DefSym.1`[A, G[B]], tpe: `Type.Expr: *→*→* * (*→* *)`[Function1, G, A, B], impl: Option[`Value.Expr: *→* *`[G, B]]) extends `Value.Expr: *→*→* * (*→* *)`[Function1, G, A, B]

sealed trait `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[F[_, _], G[_[_], _], H[_], A, B] extends `Value: *`[F[A, G[H, B]]]:
  def tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[F, G, H, A, B]
case class `Value.Def.1: *→*→* * ((*→*)→*→* *→* *)`[G[_[_], _], H[_], A, B](lvl: Int, sym: `DefSym.1`[A, G[H, B]], tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B], impl: Option[`Value.Expr: (*→*)→*→* *→* *`[G, H, B]]) extends `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B]
case class `Value.Val: *→*→* * ((*→*)→*→* *→* *)`[F[_, _], G[_[_], _], H[_], A, B](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→* *)`[F, G, H, A, B], impl: Option[`Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[F, G, H, A, B]]) extends `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[F, G, H, A, B]

sealed trait `Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F[_, _], G[_[_], _], H[_], I[_], A, B] extends `Value: *`[F[A, I[G[H, B]]]]:
  def tpe: `Type.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F, G, H, I, A, B]
case class `Value.Def.1: *→*→* * (*→* ((*→*)→*→* *→* *))`[G[_[_], _], H[_], I[_], A, B](lvl: Int, sym: `DefSym.1`[A, I[G[H, B]]], tpe: `Type.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, I, A, B], impl: Option[`Value.Expr: *→* ((*→*)→*→* *→* *)`[I, G, H, B]]) extends `Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, I, A, B]
case class `Value.Val: *→*→* * (*→* ((*→*)→*→* *→* *))`[F[_, _], G[_[_], _], H[_], I[_], A, B](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F, G, H, I, A, B], impl: Option[`Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F, G, H, I, A, B]]) extends `Value.Expr: *→*→* * (*→* ((*→*)→*→* *→* *))`[F, G, H, I, A, B]

sealed trait `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F[_, _], G[_], H[_], I[_[_], _], A, B] extends `Value: *`[F[G[A], I[H, B]]]:
  def tpe: `Type.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F, G, H, I, A, B]
case class `Value.Def.0: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F[_, _], G[_], H[_], I[_[_], _], A, B](lvl: Int, sym: `DefSym.0`, tpe: `Type.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F, G, H, I, A, B], impl: Option[`Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F, G, H, I, A, B]]) extends `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[F, G, H, I, A, B]

sealed trait `Value.Expr: (*→*)→*→* *→* *`[T[_[_], _], F[_], A] extends `Value: *`[T[F, A]]:
  def tpe: `Type.Expr: (*→*)→*→* *→* *`[T, F, A]
// case class `Value.App.1: (*→*)→*→* *→* *`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B], arg: `Value.Expr: *→* *`[G, A], tpe: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]) extends `Value.Expr: (*→*)→*→* *→* *`[I, H, B]
case class `Value.App.1: (*→*)→*→* *→* *`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B], arg: `Value: *`[A], tpe: `Type.Expr: (*→*)→*→* *→* *`[G, H, B]) extends `Value.Expr: (*→*)→*→* *→* *`[G, H, B]
case class `Value.AppDot.0: (*→*)→*→* *→* *`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B], arg1: `Value.Expr: *→* *`[G, A], tpe: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]) extends `Value.Expr: (*→*)→*→* *→* *`[I, H, B]
case class `Value.AppDot.1: (*→*)→*→* *→* *`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value.Expr: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B], targ1: `Type.Expr: (*→*)→*→* *→*`[G, H], arg1: `Value.Expr: *`[A], tpe: `Type.Expr: (*→*)→*→* *→* *`[G, H, B]) extends `Value.Expr: (*→*)→*→* *→* *`[G, H, B]
case class `Value.Val: (*→*)→*→* *→* *`[T[_[_], _], F[_], A](lvl: Int, sym: ValSym, tpe: `Type.Expr: (*→*)→*→* *→* *`[T, F, A], impl: Option[`Value.Expr: (*→*)→*→* *→* *`[T, F, A]]) extends `Value.Expr: (*→*)→*→* *→* *`[T, F, A]


sealed trait `Value.Expr: *→*→*→* * * *`[F[_, _, _], A, B, C] extends `Value: *`[F[A, B, C]]:
  def tpe: `Type.Expr: *→*→*→* * * *`[F, A, B, C]
case class `Value.Lam.2: *→*→*→* * * *`[A, B, C](lvl: Int, a1: `Value: *`[A], a2: `Value: *`[B], r: `Value: *`[C], tpe: `Type.Expr: *→*→*→* * * *`[Function2, A, B, C]) extends `Value.Expr: *→*→*→* * * *`[Function2, A, B, C]

sealed trait `Value.Expr: *→*→*→*→* * * * *`[F[_, _, _, _], A, B, C, D] extends `Value: *`[F[A, B, C, D]]:
  def tpe: `Type.Expr: *→*→*→*→* * * * *`[F, A, B, C, D]
case class `Value.Lam.3: *→*→*→*→* * * * *`[A, B, C, D](lvl: Int, a1: `Value: *`[A], a2: `Value: *`[B], a3: `Value: *`[C], r: `Value: *`[D], tpe: `Type.Expr: *→*→*→*→* * * * *`[Function3, A, B, C, D]) extends `Value.Expr: *→*→*→*→* * * * *`[Function3, A, B, C, D]

sealed trait `Value.Expr: *→*`[T[_]] extends `Value: *→*`[T]:
  def tpe: `Type.Expr: *→*`[T]
case class `Value.Val: *→*`[T[_]](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*`[T], impl: Option[`Value.Expr: *→*`[T]]) extends `Value.Expr: *→*`[T]

sealed trait `Value.Expr: (*→*)→*→* *→*`[T[_[_], _], F[_]] extends `Value: *→*`[[A] =>> T[F, A]]:
  def tpe: `Type.Expr: (*→*)→*→* *→*`[T, F]
case class `Value.App.1: (*→*)→*→* *→*`[G[_[_], _], H[_], A](lvl: Int, fun: `Value.Expr: *→*→* * ((*→*)→*→* *→*)`[Function1, G, H, A], targ: `Type.Expr: *→*`[H], arg: `Value.Expr: *`[A], tpe: `Type.Expr: (*→*)→*→* *→*`[G, H]) extends `Value.Expr: (*→*)→*→* *→*`[G, H]
case class `Value.AppDot.0: (*→*)→*→* *→*`[G[_], H[_], I[_[_], _], A, B](lvl: Int, fun: `Value.Expr: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B], targ: `Type.Expr: *→*`[H], arg1: `Value.Expr: *→* *`[G, A], tpe: `Type.Expr: (*→*)→*→* *→*`[I, H]) extends `Value.Expr: (*→*)→*→* *→*`[I, H]
case class `Value.Val: (*→*)→*→* *→*`[T[_[_], _], F[_], A](lvl: Int, sym: ValSym, targ: `Type.Expr: *→*`[F], tpe: `Type.Expr: (*→*)→*→* *→*`[T, F], impl: Option[`Value.Expr: (*→*)→*→* *→*`[T, F]]) extends `Value.Expr: (*→*)→*→* *→*`[T, F]

sealed trait `Value.Expr: *→*→* * ((*→*)→*→* *→*)`[F[_, _], G[_[_], _], H[_], A] extends `Value: *→*`[[B] =>> F[A, G[H, B]]]:
  def tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→*)`[F, G, H, A]
// case class `Value.Def.1: *→*→* * ((*→*)→*→* *→*)`[G[_[_], _], H[_], A](lvl: Int, sym: `DefSym.1`[A, [B] =>> G[H, B]], tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→*)`[Function1, G, H, A], impl: Option[`Value.Expr: (*→*)→*→* *→*`[G, H]]) extends `Value.Expr: *→*→* * ((*→*)→*→* *→*)`[Function1, G, H, A]
case class `Value.Val: *→*→* * ((*→*)→*→* *→*)`[F[_, _], G[_[_], _], H[_], A](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*→* * ((*→*)→*→* *→*)`[F, G, H, A], impl: Option[`Value.Expr: *→*→* * ((*→*)→*→* *→*)`[F, G, H, A]]) extends `Value.Expr: *→*→* * ((*→*)→*→* *→*)`[F, G, H, A]

sealed trait `Value.Expr: (*→*)→*`[T[_[_]]] extends `Value: (*→*)→*`[T]:
  def tpe: `Type.Expr: (*→*)→*`[T]
case class `Value.Val: (*→*)→*`[T[_[_]]](lvl: Int, sym: ValSym, tpe: `Type.Expr: (*→*)→*`[T], impl: Option[`Value.Expr: (*→*)→*`[T]]) extends `Value.Expr: (*→*)→*`[T]

sealed trait `Value.Expr: *→*→*`[T[_, _]] extends `Value: *→*→*`[T]:
  def tpe: `Type.Expr: *→*→*`[T]
case class `Value.Val: *→*→*`[T[_, _]](lvl: Int, sym: ValSym, tpe: `Type.Expr: *→*→*`[T], impl: Option[`Value.Expr: *→*→*`[T]]) extends `Value.Expr: *→*→*`[T]

sealed trait `Value.Expr: (*→*)→*→*`[T[_[_], _]] extends `Value: (*→*)→*→*`[T]:
  def tpe: `Type.Expr: (*→*)→*→*`[T]
case class `Value.Val: (*→*)→*→*`[T[_[_], _]](lvl: Int, sym: ValSym, tpe: `Type.Expr: (*→*)→*→*`[T], impl: Option[`Value.Expr: (*→*)→*→*`[T]]) extends `Value.Expr: (*→*)→*→*`[T]

sealed trait `Value.Expr: (*→*)→*→*→*`[T[_[_], _, _]] extends `Value: (*→*)→*→*→*`[T]:
  def tpe: `Type.Expr: (*→*)→*→*→*`[T]
case class `Value.Val: (*→*)→*→*→*`[T[_[_], _, _]](lvl: Int, sym: ValSym, tpe: `Type.Expr: (*→*)→*→*→*`[T], impl: Option[`Value.Expr: (*→*)→*→*→*`[T]]) extends `Value.Expr: (*→*)→*→*→*`[T]