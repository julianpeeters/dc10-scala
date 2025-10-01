package dc10.scala

sealed trait Type

sealed trait `Type.Expr: *`[+T] extends Type
sealed trait `Type: *→*`[+T[_]] extends Type
sealed trait `Type: *→*→*`[T[_, _]] extends Type
sealed trait `Type.*→*→*→*`[T[_, _, _]] extends Type
sealed trait `Type.*→*→*→*→*`[T[_, _, _, _]] extends Type
sealed trait `Type: (*→*)→*`[T[_[_]]] extends Type
sealed trait `Type: (*→*)→*→*`[T[_[_], _]] extends Type
sealed trait `Type.(*→*)→*→*→*`[T[_[_], _, _]] extends Type
sealed trait `Type.((*→*)→*→*)→*`[T[_[_[_], _]]] extends Type

case class `Type.App[_]`[T[_], A](lvl: Int, tfun: `Type: *→*`[T], aarg: `Type.Expr: *`[A]) extends `Type.Expr: *`[T[A]]
case class `Type.App[_[_]]`[T[_[_]], F[_]](lvl: Int, tfun: `Type: (*→*)→*`[T], farg: `Type.Expr: *→*`[F]) extends `Type.Expr: *`[T[F]]
case class `Type.App[_[_[_], _]]`[T[_[_[_], _]], F[_[_], _]](lvl: Int, tfun: `Type.((*→*)→*→*)→*`[T], farg: `Type: (*→*)→*→*`[F]) extends `Type.Expr: *`[T[F]]
case class `Type.App[_, _]`[T[_,_], A, B](lvl: Int, tfun: `Type: *→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B]) extends `Type.Expr: *`[T[A, B]]
case class `Type.App[_[_], _]`[T[_[_], _], F[_], A](lvl: Int, tfun: `Type: (*→*)→*→*`[T], farg: `Type.Expr: *→*`[F], aarg: `Type.Expr: *`[A]) extends `Type.Expr: *`[T[F, A]]
case class `Type.App[_, _, _]`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.*→*→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B], carg: `Type.Expr: *`[C]) extends `Type.Expr: *`[T[A, B, C]]
case class `Type.App[_[_], _, _]`[T[_[_], _, _], F[_], A, B](lvl: Int, tfun: `Type.(*→*)→*→*→*`[T], farg: `Type.Expr: *→*`[F], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B]) extends `Type.Expr: *`[T[F, A, B]]
case class `Type.App[_, _, _, _]`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.*→*→*→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B], carg: `Type.Expr: *`[C], darg: `Type.Expr: *`[D]) extends `Type.Expr: *`[T[A, B, C, D]]
case class `Type.AppInfix[_, _]`[T[_,_], A, B](lvl: Int, tfun: `Type: *→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B]) extends `Type.Expr: *`[T[A, B]]
case class `Type.AppInfix[_, _, _]`[T[_,_,_], A, B, C](lvl: Int, tfun: `Type.*→*→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B], carg: `Type.Expr: *`[C]) extends `Type.Expr: *`[T[A, B, C]]
case class `Type.AppInfix[_, _, _, _]`[T[_,_,_,_], A, B, C, D](lvl: Int, tfun: `Type.*→*→*→*→*`[T], aarg: `Type.Expr: *`[A], barg: `Type.Expr: *`[B], carg: `Type.Expr: *`[C], darg: `Type.Expr: *`[D]) extends `Type.Expr: *`[T[A, B, C, D]]

case class `Type.Lam: *→*`[F[_], A](lvl: Int, domain: `Type.Var: *`[A], codomain: `Type.Expr: *`[F[A]]) extends `Type.Expr: *→*`[F]
case class `Type.Lam: (*→*)→*→*`[F[_[_], _], G[_], A](lvl: Int, domain1: `Type.Var: *→*`[G], domain2: `Type.Var: *`[A], codomain: `Type.Expr: *`[F[G, A]]) extends `Type: (*→*)→*→*`[F]


case class `Type.Var: *`[+T](lvl: Int, nme: String, impl: Option[`Type.Expr: *`[T]]) extends `Type.Expr: *`[T]


case class `Type.Var: *→*`[T[_]](lvl: Int, nme: String, impl: Option[`Type: *→*`[[A] =>> T[A]]], ctors: () => List[Value]) extends `Type: *→*`[[A] =>> T[A]]
// case class `Type.Var2[_]`[T[_]](lvl: Int, nme: String, impl: Option[`Type: *→*`[[A] =>> T[A]]], ctor1: `Value.*→*`[[A] =>> A => T[A]], ctor2: `Value.*→*`[[A] =>> A => T[A]]) extends `Type: *→*`[[A] =>> T[A]]
case class `Type.Var: (*→*)→*`[T[_[_]]](lvl: Int, nme: String, impl: Option[`Type: (*→*)→*`[T]]) extends `Type: (*→*)→*`[T]
case class `Type.Var: *→*→*`[T[_, _]](lvl: Int, nme: String, impl: Option[`Type: *→*→*`[T]]) extends `Type: *→*→*`[T]
case class `Type.Var: (*→*)→*→*`[T[_[_], _]](lvl: Int, nme: String, impl: Option[`Type: (*→*)→*→*`[T]]) extends `Type: (*→*)→*→*`[T]
case class `Type.Var: *→*→*→*`[T[_, _, _]](lvl: Int, nme: String, impl: Option[`Type.*→*→*→*`[T]]) extends `Type.*→*→*→*`[T]
case class `Type.Var: (*→*)→*→*→*`[T[_[_], _, _]](lvl: Int, nme: String, impl: Option[`Type.(*→*)→*→*→*`[T]]) extends `Type.(*→*)→*→*→*`[T]
case class `Type.Var: ((*→*)→*→*)→*`[T[_[_[_], _]]](lvl: Int, nme: String, impl: Option[`Type.((*→*)→*→*)→*`[T]]) extends `Type.((*→*)→*→*)→*`[T]
case class `Type.Var: *→*→*→*→*`[T[_, _, _, _]](lvl: Int, nme: String, impl: Option[`Type.*→*→*→*→*`[T]]) extends `Type.*→*→*→*→*`[T]



case class `Type.Bot: *`(in: Int) extends `Type.Expr: *`[Nothing]