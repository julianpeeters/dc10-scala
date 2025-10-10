package dc10.scala

sealed trait Value

sealed trait `Value: x`[T] extends Value:
  def tpe: `Type: x`[T]

sealed trait `Value.x→x`[T[_]] extends Value:
  def tpe: `Type: x→x`[T]

sealed trait `Value.(x→x)→x`[T[_[_]]] extends Value:
  def tpe: `Type: (x→x)→x`[T]

sealed trait `Value.x→x→x`[T[_, _]] extends Value:
  def tpe: `Type: x→x→x`[T]

sealed trait `Value.(x→x)→x→x`[T[_[_], _]] extends Value:
  def tpe: `Type: (x→x)→x→x`[T]

sealed trait `Value.(x→x)→x→x→x`[T[_[_], _, _]] extends Value:
  def tpe: `Type.(x→x)→x→x→x`[T]

case class `Value.App.1: x`[A, B](lvl: Int, fun: `Value: x`[A => B], arg: `Value: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.App.2: x`[A, B, C](lvl: Int, fun: `Value: x`[(A, B) => C], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.App.3: x`[A, B, C, D](lvl: Int, fun: `Value: x`[(A, B, C) => D], arg1: `Value: x`[A], arg2: `Value: x`[B], arg3: `Value: x`[C], tpe: `Type: x`[D]) extends `Value: x`[D]
case class `Value.App.Vargs: x`[A, B](lvl: Int, fun: `Value: x`[List[A] => B], tpe: `Type: x`[B], vargs: `Value: x`[A]x) extends `Value: x`[B]
case class `Value.AppDot.0: x`[A, B](lvl: Int, fun: `Value: x`[A => B], arg1: `Value: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.AppDot.1: x`[A, B, C, D](lvl: Int, fun: `Value: x`[D], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.AppDotless: x`[A, B, C, D](lvl: Int, fun: `Value: x`[D], arg1: `Value: x`[A], arg2: `Value: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]
case class `Value.AppForComp: x→x x`[G[_], A](lvl: Int, gens: List[Statement], ret: `Value: x`[A], tpe: `Type: x`[G[A]]) extends `Value: x`[G[A]]
case class `Value.App.Match`[A, B](lvl: Int, value: `Value: x`[A], tpe: `Type: x`[B], cases: List[Statement.`case`[A => B]]) extends `Value: x`[B]
case class `Value.App.0: x→x`[G[_], A, B](lvl: Int, fun: `Value.x→x`[G], targ: `Type: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.App.0: (x→x)→x→x`[G[_[_], _], H[_], A, B](lvl: Int, fun: `Value.(x→x)→x→x`[G], targf: `Type: x→x`[H], targa: `Type: x`[A], tpe: `Type: x`[B]) extends `Value: x`[B]
case class `Value.App.0: (x→x)→x→x`[G[_[_], _, _], H[_], A, B, C](lvl: Int, fun: `Value.(x→x)→x→x→x`[G], targf: `Type: x→x`[H], targa: `Type: x`[A], targb: `Type: x`[B], tpe: `Type: x`[C]) extends `Value: x`[C]



case class `Value.Lam.1: x→x→x x x`[A, B](lvl: Int, a: `Value: x`[A], b: `Value: x`[B], tpe: `Type: x`[A => B]) extends `Value: x`[A => B]
case class `Value.Lam.2: x→x→x→x x * x`[A, B, C](lvl: Int, a1: `Value: x`[A], a2: `Value: x`[B], r: `Value: x`[C], tpe: `Type: x`[(A, B) => C]) extends `Value: x`[(A, B) => C]
case class `Value.Lam.3: x→x→x→x→x x * x x`[A, B, C, D](lvl: Int, a1: `Value: x`[A], a2: `Value: x`[B], a3: `Value: x`[C], r: `Value: x`[D], tpe: `Type: x`[(A, B, C) => D]) extends `Value: x`[(A, B, C) => D]
case class `Value.Lam.1: x→x`[F[_], A](lvl: Int, fa: `Value: x`[F[A]], tpe: `Type: x→x`[F]) extends `Value.x→x`[F]


sealed trait `Value.Lit`[T] extends `Value: x`[T]
case class `Value.Lit.Boolean: x`(lvl: Int, tpe: `Type: x`[Boolean], b: Boolean) extends `Value.Lit`[Boolean]
case class `Value.Lit.Int: x`(lvl: Int, tpe: `Type: x`[Int], i: Int) extends `Value.Lit`[Int]
case class `Value.Lit.String: x`(lvl: Int, tpe: `Type: x`[String], s: String) extends `Value.Lit`[String]
case class `Value.Lit.Unit: x`(lvl: Int, tpe: `Type: x`[Unit], u: Unit) extends `Value.Lit`[Unit]

sealed trait `Value.Var`[T] extends `Value: x`[T]
sealed trait `Value.Var.Unbound`[T] extends `Value.Var`[T]
sealed trait `Value.Var.Bound`[T] extends `Value.Var`[T]

case class `Value.Var.Unbound.Data`[T](lvl: Int, nme: String, tpe: `Type: x`[T]) extends `Value.Var.Unbound`[T]
case class `Value.Var.Bound.Data`[T](lvl: Int, nme: String, tpe: `Type: x`[T], impl: `Value: x`[T]) extends `Value.Var.Bound`[T]
case class `Value.Var0[_]`[T[_]](lvl: Int, nme: String, tpe: `Type: x→x`[[A] =>> T[A]], impl: Option[`Value.x→x`[[A] =>> T[A]]]) extends `Value.x→x`[[A] =>> T[A]]
case class `Value.Var1[_]`[F[_], G[_]](lvl: Int, nme: String, tpe: `Type: x→x`[[A] =>> F[A] => G[A]], impl: Option[`Value.x→x`[[A] =>> F[A] => G[A]]]) extends `Value.x→x`[[A] =>> F[A] => G[A]]
case class `Value.Var[_[_]]`[T[_[_]]](lvl: Int, nme: String, tpe: `Type: (x→x)→x`[T], impl: Option[`Value.(x→x)→x`[T]]) extends `Value.(x→x)→x`[T]
case class `Value.Var[_, _]`[T[_, _]](lvl: Int, nme: String, tpe: `Type: x→x→x`[T], impl: Option[`Value.x→x→x`[T]]) extends `Value.x→x→x`[T]
case class `Value.Var[_[_], _]`[T[_[_], _]](lvl: Int, nme: String, tpe: `Type: (x→x)→x→x`[T], impl: Option[`Value.(x→x)→x→x`[T]]) extends `Value.(x→x)→x→x`[T]
case class `Value.Var[_[_], _, _]`[T[_[_], _, _]](lvl: Int, nme: String, tpe: `Type.(x→x)→x→x→x`[T], impl: Option[`Value.(x→x)→x→x→x`[T]]) extends `Value.(x→x)→x→x→x`[T]
