package dc10.scala

sealed trait Type

sealed trait `Type: x`[+T] extends Type
sealed trait `Type: x→x`[+T[_]] extends Type
sealed trait `Type: x→x→x`[T[_, _]] extends Type
sealed trait `Type.x→x→x→x`[T[_, _, _]] extends Type
sealed trait `Type.x→x→x→x→x`[T[_, _, _, _]] extends Type
sealed trait `Type: (x→x)→x`[T[_[_]]] extends Type
sealed trait `Type: (x→x)→x→x`[T[_[_], _]] extends Type
sealed trait `Type.(x→x)→x→x→x`[T[_[_], _, _]] extends Type
sealed trait `Type.((x→x)→x→x)→x`[T[_[_[_], _]]] extends Type

case class `Type.App[_]`[T[_], A](lvl: Int, tfun: `Type: x→x`[T], aarg: `Type: x`[A]) extends `Type: x`[T[A]]
case class `Type.App[_[_]]`[T[_[_]], F[_]](lvl: Int, tfun: `Type: (x→x)→x`[T], farg: `Type: x→x`[F]) extends `Type: x`[T[F]]
case class `Type.App[_[_[_], _]]`[T[_[_[_], _]], F[_[_], _]](lvl: Int, tfun: `Type.((x→x)→x→x)→x`[T], farg: `Type: (x→x)→x→x`[F]) extends `Type: x`[T[F]]
case class `Type.App[_, _]`[T[_,_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B]) extends `Type: x`[T[A, B]]
case class `Type.App[_[_], _]`[T[_[_], _], F[_], A](lvl: Int, tfun: `Type: (x→x)→x→x`[T], farg: `Type: x→x`[F], aarg: `Type: x`[A]) extends `Type: x`[T[F, A]]
case class `Type.App[_, _, _]`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.x→x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B], carg: `Type: x`[C]) extends `Type: x`[T[A, B, C]]
case class `Type.App[_[_], _, _]`[T[_[_], _, _], F[_], A, B](lvl: Int, tfun: `Type.(x→x)→x→x→x`[T], farg: `Type: x→x`[F], aarg: `Type: x`[A], barg: `Type: x`[B]) extends `Type: x`[T[F, A, B]]
case class `Type.App[_, _, _, _]`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.x→x→x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B], carg: `Type: x`[C], darg: `Type: x`[D]) extends `Type: x`[T[A, B, C, D]]
case class `Type.AppInfix[_, _]`[T[_,_], A, B](lvl: Int, tfun: `Type: x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B]) extends `Type: x`[T[A, B]]
case class `Type.AppInfix[_, _, _]`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.x→x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B], carg: `Type: x`[C]) extends `Type: x`[T[A, B, C]]
case class `Type.AppInfix[_, _, _, _]`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.x→x→x→x→x`[T], aarg: `Type: x`[A], barg: `Type: x`[B], carg: `Type: x`[C], darg: `Type: x`[D]) extends `Type: x`[T[A, B, C, D]]

case class `Type.Lam: x→x`[F[_], A](lvl: Int, domain: `Type.Var: x`[A], codomain: `Type: x`[F[A]]) extends `Type: x→x`[F]
case class `Type.Lam: (x→x)→x→x`[F[_[_], _], G[_], A](lvl: Int, domain1: `Type.Var: x→x`[G], domain2: `Type.Var: x`[A], codomain: `Type: x`[F[G, A]]) extends `Type: (x→x)→x→x`[F]


case class `Type.Var: x`[+T](lvl: Int, nme: String, impl: Option[`Type: x`[T]]) extends `Type: x`[T]


case class `Type.Var: x→x`[T[_]](lvl: Int, nme: String, impl: Option[`Type: x→x`[[A] =>> T[A]]], ctors: () => List[Value]) extends `Type: x→x`[[A] =>> T[A]]
// case class `Type.Var2[_]`[T[_]](lvl: Int, nme: String, impl: Option[`Type: x→x`[[A] =>> T[A]]], ctor1: `Value.x→x`[[A] =>> A => T[A]], ctor2: `Value.x→x`[[A] =>> A => T[A]]) extends `Type: x→x`[[A] =>> T[A]]
case class `Type.Var: (x→x)→x`[T[_[_]]](lvl: Int, nme: String, impl: Option[`Type: (x→x)→x`[T]]) extends `Type: (x→x)→x`[T]
case class `Type.Var: x→x→x`[T[_, _]](lvl: Int, nme: String, impl: Option[`Type: x→x→x`[T]]) extends `Type: x→x→x`[T]
case class `Type.Var: (x→x)→x→x`[T[_[_], _]](lvl: Int, nme: String, impl: Option[`Type: (x→x)→x→x`[T]]) extends `Type: (x→x)→x→x`[T]
case class `Type.Var: x→x→x→x`[T[_, _, _]](lvl: Int, nme: String, impl: Option[`Type.x→x→x→x`[T]]) extends `Type.x→x→x→x`[T]
case class `Type.Var: (x→x)→x→x→x`[T[_[_], _, _]](lvl: Int, nme: String, impl: Option[`Type.(x→x)→x→x→x`[T]]) extends `Type.(x→x)→x→x→x`[T]
case class `Type.Var: ((x→x)→x→x)→x`[T[_[_[_], _]]](lvl: Int, nme: String, impl: Option[`Type.((x→x)→x→x)→x`[T]]) extends `Type.((x→x)→x→x)→x`[T]
case class `Type.Var: x→x→x→x→x`[T[_, _, _, _]](lvl: Int, nme: String, impl: Option[`Type.x→x→x→x→x`[T]]) extends `Type.x→x→x→x→x`[T]



case class `Type.Bot: x`(in: Int) extends `Type: x`[Nothing]