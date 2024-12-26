package dc10.scala

sealed trait Value

sealed trait `Value.*`[T] extends Value:
  def tpe: `Type.*`[T]

sealed trait `Value.*->*`[T[_]] extends Value:
  def tpe: `Type.*->*`[T]

sealed trait `Value.(*->*)->*`[T[_[_]]] extends Value:
  def tpe: `Type.(*->*)->*`[T]

sealed trait `Value.*->*->*`[T[_, _]] extends Value:
  def tpe: `Type.*->*->*`[T]

sealed trait `Value.(*->*)->*->*`[T[_[_], _]] extends Value:
  def tpe: `Type.(*->*)->*->*`[T]

case class `Value.App1`[A, B](in: Int, fun: `Value.*`[A => B], arg: `Value.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]
case class `Value.App2`[A, B, C](in: Int, fun: `Value.*`[(A, B) => C], arg: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
case class `Value.App3`[A, B, C, D](in: Int, fun: `Value.*`[(A, B, C) => D], arg: `Value.*`[A], arg2: `Value.*`[B], arg3: `Value.*`[C], tpe: `Type.*`[D]) extends `Value.*`[D]
case class `Value.AppVargs`[A, B](in: Int, fun: `Value.*`[List[A] => B], tpe: `Type.*`[B], vargs: `Value.*`[A]*) extends `Value.*`[B]
case class `Value.AppDot0`[A, B](in: Int, fun: `Value.*`[A => B], arg1: `Value.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]
case class `Value.AppDot1`[A, B, C, D](in: Int, fun: `Value.*`[D], arg1: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
case class `Value.AppDotless`[A, B, C, D](in: Int, fun: `Value.*`[D], arg1: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
case class `Value.AppForComp`[G[_], A](in: Int, gens: List[Statement], ret: `Value.*`[A], tpe: `Type.*`[G[A]]) extends `Value.*`[G[A]]
case class `Value.AppType`[G[_], A, B](in: Int, fun: `Value.*->*`[G], targ: `Type.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]

case class `Value.LitBoolean`(in: Int, tpe: `Type.*`[Boolean], b: Boolean) extends `Value.*`[Boolean]
case class `Value.LitInt`(in: Int, tpe: `Type.*`[Int], i: Int) extends `Value.*`[Int]
case class `Value.LitString`(in: Int, tpe: `Type.*`[String], s: String) extends `Value.*`[String]
case class `Value.LitUnit`(in: Int, tpe: `Type.*`[Unit], u: Unit) extends `Value.*`[Unit]

case class `Value.Lam1`[A, B](in: Int, a: `Value.*`[A], b: `Value.*`[B], tpe: `Type.*`[A => B]) extends `Value.*`[A => B]
case class `Value.Lam2`[A, B, C](in: Int, a1: `Value.*`[A], a2: `Value.*`[B], r: `Value.*`[C], tpe: `Type.*`[(A, B) => C]) extends `Value.*`[(A, B) => C]
case class `Value.Lam3`[A, B, C, D](in: Int, a1: `Value.*`[A], a2: `Value.*`[B], a3: `Value.*`[C], r: `Value.*`[D], tpe: `Type.*`[(A, B, C) => D]) extends `Value.*`[(A, B, C) => D]

sealed trait `Value.Var`[T] extends `Value.*`[T]
sealed trait `Value.VarAbstract`[T] extends `Value.Var`[T]
sealed trait `Value.VarConcrete`[T] extends `Value.Var`[T]
case class `Value.VarA`[T](in: Int, nme: String, tpe: `Type.*`[T]) extends `Value.VarAbstract`[T]
case class `Value.VarC`[T](in: Int, nme: String, tpe: `Type.*`[T], impl: `Value.*`[T]) extends `Value.VarConcrete`[T]
case class `Value.Var0[_]`[T[_]](in: Int, nme: String, tpe: `Type.*->*`[[A] =>> T[A]], impl: Option[`Value.*->*`[[A] =>> T[A]]]) extends `Value.*->*`[[A] =>> T[A]]
case class `Value.Var1[_]`[F[_], G[_]](in: Int, nme: String, tpe: `Type.*->*`[[A] =>> F[A] => G[A]], impl: Option[`Value.*->*`[[A] =>> F[A] => G[A]]]) extends `Value.*->*`[[A] =>> F[A] => G[A]]
case class `Value.Var[_[_]]`[T[_[_]]](in: Int, nme: String, tpe: `Type.(*->*)->*`[T], impl: Option[`Value.(*->*)->*`[T]]) extends `Value.(*->*)->*`[T]
case class `Value.Var[_, _]`[T[_, _]](in: Int, nme: String, tpe: `Type.*->*->*`[T], impl: Option[`Value.*->*->*`[T]]) extends `Value.*->*->*`[T]
case class `Value.Var[_[_], _]`[T[_[_], _]](in: Int, nme: String, tpe: `Type.(*->*)->*->*`[T], impl: Option[`Value.(*->*)->*->*`[T]]) extends `Value.(*->*)->*->*`[T]