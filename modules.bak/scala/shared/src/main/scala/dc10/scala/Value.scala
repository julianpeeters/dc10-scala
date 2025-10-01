package dc10.scala

sealed trait Value

sealed trait `Value.Expr: *`[T] extends Value:
  def tpe: `Type.Expr: *`[T]

sealed trait `Value.*→*`[T[_]] extends Value:
  def tpe: `Type: *→*`[T]

sealed trait `Value.(*→*)→*`[T[_[_]]] extends Value:
  def tpe: `Type: (*→*)→*`[T]

sealed trait `Value.*→*→*`[T[_, _]] extends Value:
  def tpe: `Type: *→*→*`[T]

sealed trait `Value.(*→*)→*→*`[T[_[_], _]] extends Value:
  def tpe: `Type: (*→*)→*→*`[T]

sealed trait `Value.(*→*)→*→*→*`[T[_[_], _, _]] extends Value:
  def tpe: `Type.(*→*)→*→*→*`[T]

case class `Value.App.1: *`[A, B](lvl: Int, fun: `Value.Expr: *`[A => B], arg: `Value.Expr: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.2: *`[A, B, C](lvl: Int, fun: `Value.Expr: *`[(A, B) => C], arg1: `Value.Expr: *`[A], arg2: `Value.Expr: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.App.3: *`[A, B, C, D](lvl: Int, fun: `Value.Expr: *`[(A, B, C) => D], arg1: `Value.Expr: *`[A], arg2: `Value.Expr: *`[B], arg3: `Value.Expr: *`[C], tpe: `Type.Expr: *`[D]) extends `Value.Expr: *`[D]
case class `Value.App.Vargs: *`[A, B](lvl: Int, fun: `Value.Expr: *`[List[A] => B], tpe: `Type.Expr: *`[B], vargs: `Value.Expr: *`[A]*) extends `Value.Expr: *`[B]
case class `Value.AppDot.0: *`[A, B](lvl: Int, fun: `Value.Expr: *`[A => B], arg1: `Value.Expr: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.AppDot.1: *`[A, B, C, D](lvl: Int, fun: `Value.Expr: *`[D], arg1: `Value.Expr: *`[A], arg2: `Value.Expr: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.AppDotless: *`[A, B, C, D](lvl: Int, fun: `Value.Expr: *`[D], arg1: `Value.Expr: *`[A], arg2: `Value.Expr: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]
case class `Value.AppForComp: *→* *`[G[_], A](lvl: Int, gens: List[Statement], ret: `Value.Expr: *`[A], tpe: `Type.Expr: *`[G[A]]) extends `Value.Expr: *`[G[A]]
case class `Value.App.Match`[A, B](lvl: Int, value: `Value.Expr: *`[A], tpe: `Type.Expr: *`[B], cases: List[Statement.`case`[A => B]]) extends `Value.Expr: *`[B]
case class `Value.App.0: *→*`[G[_], A, B](lvl: Int, fun: `Value.*→*`[G], targ: `Type.Expr: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.0: (*→*)→*→*`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value.(*→*)→*→*`[G], targf: `Type: *→*`[H], targa: `Type.Expr: *`[A], tpe: `Type.Expr: *`[B]) extends `Value.Expr: *`[B]
case class `Value.App.0: (*→*)→*→*`[G[_[_], _, _], H[_], A, B, C](lvl: Int, fun: `Value.(*→*)→*→*→*`[G], targf: `Type: *→*`[H], targa: `Type.Expr: *`[A], targb: `Type.Expr: *`[B], tpe: `Type.Expr: *`[C]) extends `Value.Expr: *`[C]



case class `Value.Lam.1: *→*→* * *`[A, B](lvl: Int, a: `Value.Expr: *`[A], b: `Value.Expr: *`[B], tpe: `Type.Expr: *`[A => B]) extends `Value.Expr: *`[A => B]
case class `Value.Lam.2: *→*→*→* * * *`[A, B, C](lvl: Int, a1: `Value.Expr: *`[A], a2: `Value.Expr: *`[B], r: `Value.Expr: *`[C], tpe: `Type.Expr: *`[(A, B) => C]) extends `Value.Expr: *`[(A, B) => C]
case class `Value.Lam.3: *→*→*→*→* * * * *`[A, B, C, D](lvl: Int, a1: `Value.Expr: *`[A], a2: `Value.Expr: *`[B], a3: `Value.Expr: *`[C], r: `Value.Expr: *`[D], tpe: `Type.Expr: *`[(A, B, C) => D]) extends `Value.Expr: *`[(A, B, C) => D]
case class `Value.Lam.1: *→*`[F[_], A](lvl: Int, fa: `Value.Expr: *`[F[A]], tpe: `Type: *→*`[F]) extends `Value.*→*`[F]


sealed trait `Value.Lit`[T] extends `Value.Expr: *`[T]
case class `Value.Lit.Boolean: *`(lvl: Int, tpe: `Type.Expr: *`[Boolean], b: Boolean) extends `Value.Lit`[Boolean]
case class `Value.Lit.Int: *`(lvl: Int, tpe: `Type.Expr: *`[Int], i: Int) extends `Value.Lit`[Int]
case class `Value.Lit.String: *`(lvl: Int, tpe: `Type.Expr: *`[String], s: String) extends `Value.Lit`[String]
case class `Value.Lit.Unit: *`(lvl: Int, tpe: `Type.Expr: *`[Unit], u: Unit) extends `Value.Lit`[Unit]

sealed trait `Value.Var`[T] extends `Value.Expr: *`[T]
sealed trait `Value.Var.Unbound`[T] extends `Value.Var`[T]
sealed trait `Value.Var.Bound`[T] extends `Value.Var`[T]

case class `Value.Var.Unbound.Data`[T](lvl: Int, nme: String, tpe: `Type.Expr: *`[T]) extends `Value.Var.Unbound`[T]
case class `Value.Var.Bound.Data`[T](lvl: Int, nme: String, tpe: `Type.Expr: *`[T], impl: `Value.Expr: *`[T]) extends `Value.Var.Bound`[T]
case class `Value.Var0[_]`[T[_]](lvl: Int, nme: String, tpe: `Type: *→*`[[A] =>> T[A]], impl: Option[`Value.*→*`[[A] =>> T[A]]]) extends `Value.*→*`[[A] =>> T[A]]
case class `Value.Var1[_]`[F[_], G[_]](lvl: Int, nme: String, tpe: `Type: *→*`[[A] =>> F[A] => G[A]], impl: Option[`Value.*→*`[[A] =>> F[A] => G[A]]]) extends `Value.*→*`[[A] =>> F[A] => G[A]]
case class `Value.Var[_[_]]`[T[_[_]]](lvl: Int, nme: String, tpe: `Type: (*→*)→*`[T], impl: Option[`Value.(*→*)→*`[T]]) extends `Value.(*→*)→*`[T]
case class `Value.Var[_, _]`[T[_, _]](lvl: Int, nme: String, tpe: `Type: *→*→*`[T], impl: Option[`Value.*→*→*`[T]]) extends `Value.*→*→*`[T]
case class `Value.Var[_[_], _]`[T[_[_], _]](lvl: Int, nme: String, tpe: `Type: (*→*)→*→*`[T], impl: Option[`Value.(*→*)→*→*`[T]]) extends `Value.(*→*)→*→*`[T]
case class `Value.Var[_[_], _, _]`[T[_[_], _, _]](lvl: Int, nme: String, tpe: `Type.(*→*)→*→*→*`[T], impl: Option[`Value.(*→*)→*→*→*`[T]]) extends `Value.(*→*)→*→*→*`[T]
