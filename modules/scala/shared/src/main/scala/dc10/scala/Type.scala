package dc10.scala

sealed trait Type

sealed trait `Type.*`[+T] extends Type
sealed trait `Type.*->*`[T[_]] extends Type
sealed trait `Type.*->*->*`[T[_, _]] extends Type
sealed trait `Type.*->*->*->*`[T[_, _, _]] extends Type
sealed trait `Type.*->*->*->*->*`[T[_, _, _, _]] extends Type
sealed trait `Type.(*->*)->*`[T[_[_]]] extends Type
sealed trait `Type.(*->*)->*->*`[T[_[_], _]] extends Type
sealed trait `Type.(*->*)->*->*->*`[T[_[_], _, _]] extends Type
sealed trait `Type.((*->*)->*->*)->*`[T[_[_[_], _]]] extends Type

case class `Type.App[_]`[T[_], A](in: Int, tfun: `Type.*->*`[T], aarg: `Type.*`[A]) extends `Type.*`[T[A]]
case class `Type.App[_[_]]`[T[_[_]], F[_]](in: Int, tfun: `Type.(*->*)->*`[T], farg: `Type.*->*`[F]) extends `Type.*`[T[F]]
case class `Type.App[_[_[_], _]]`[T[_[_[_], _]], F[_[_], _]](in: Int, tfun: `Type.((*->*)->*->*)->*`[T], farg: `Type.(*->*)->*->*`[F]) extends `Type.*`[T[F]]
case class `Type.App[_, _]`[T[_,_], A, B](in: Int, tfun: `Type.*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[A, B]]
case class `Type.App[_[_], _]`[T[_[_], _], F[_], A](in: Int, tfun: `Type.(*->*)->*->*`[T], farg: `Type.*->*`[F], aarg: `Type.*`[A]) extends `Type.*`[T[F, A]]
case class `Type.App[_, _, _]`[T[_,_,_], A, B, C](in: Int, tfun: `Type.*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C]) extends `Type.*`[T[A, B, C]]
case class `Type.App[_[_], _, _]`[T[_[_], _, _], F[_], A, B](in: Int, tfun: `Type.(*->*)->*->*->*`[T], farg: `Type.*->*`[F], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[F, A, B]]
case class `Type.App[_, _, _, _]`[T[_,_,_,_], A, B, C, D](in: Int, tfun: `Type.*->*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C], darg: `Type.*`[D]) extends `Type.*`[T[A, B, C, D]]
case class `Type.AppInfix[_, _]`[T[_,_], A, B](in: Int, tfun: `Type.*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B]) extends `Type.*`[T[A, B]]
case class `Type.AppInfix[_, _, _]`[T[_,_,_], A, B, C](in: Int, tfun: `Type.*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C]) extends `Type.*`[T[A, B, C]]
case class `Type.AppInfix[_, _, _, _]`[T[_,_,_,_], A, B, C, D](in: Int, tfun: `Type.*->*->*->*->*`[T], aarg: `Type.*`[A], barg: `Type.*`[B], carg: `Type.*`[C], darg: `Type.*`[D]) extends `Type.*`[T[A, B, C, D]]
case class `Type.Lam`[F[_], A](in: Int, domain: `Type.Var`[A], codomain: `Type.*`[F[A]]) extends `Type.*->*`[F]
case class `Type.Var`[T](in: Int, nme: String, impl: Option[`Type.*`[T]]) extends `Type.*`[T]
case class `Type.Var[_]`[T[_]](in: Int, nme: String, impl: Option[`Type.*->*`[T]]) extends `Type.*->*`[T]
case class `Type.Var[_[_]]`[T[_[_]]](in: Int, nme: String, impl: Option[`Type.(*->*)->*`[T]]) extends `Type.(*->*)->*`[T]
case class `Type.Var[_, _]`[T[_, _]](in: Int, nme: String, impl: Option[`Type.*->*->*`[T]]) extends `Type.*->*->*`[T]
case class `Type.Var[_[_], _]`[T[_[_], _]](in: Int, nme: String, impl: Option[`Type.(*->*)->*->*`[T]]) extends `Type.(*->*)->*->*`[T]
case class `Type.Var[_, _, _]`[T[_, _, _]](in: Int, nme: String, impl: Option[`Type.*->*->*->*`[T]]) extends `Type.*->*->*->*`[T]
case class `Type.Var[_[_], _, _]`[T[_[_], _, _]](in: Int, nme: String, impl: Option[`Type.(*->*)->*->*->*`[T]]) extends `Type.(*->*)->*->*->*`[T]
case class `Type.Var[_[_[_], _]]`[T[_[_[_], _]]](in: Int, nme: String, impl: Option[`Type.((*->*)->*->*)->*`[T]]) extends `Type.((*->*)->*->*)->*`[T]
case class `Type.Var[_, _, _, _]`[T[_, _, _, _]](in: Int, nme: String, impl: Option[`Type.*->*->*->*->*`[T]]) extends `Type.*->*->*->*->*`[T]
case class `Type.Bot`(in: Int) extends `Type.*`[Nothing]