package dc10.scala

sealed trait Statement
object Statement:
  
  case class `case`[T](lambda: `Value.Expr: *`[T]) extends Statement
  case class `case class`[T](tpe: `Type.Var: *`[T], fields: List[Statement], body: List[Statement]) extends Statement
  case class `extension`[T](field: `Value.Var.Unbound.Data`[T], body: List[Statement]) extends Statement
  object `def`:
    case class `0`[A](value: `Value.Var`[A]) extends Statement
    object `0`:
      case class `[_]`[T, A](tparam: `Type.Expr: *`[A], impl: Option[`Value.Expr: *`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_]]`[F[_], A](tparam: `Type.Expr: *→*`[F], impl: Option[`Value.Expr: *`[A]], value: `Value.Var`[A]) extends Statement
      case class `[_[_], _]`[F[_], A, T](tparamf: `Type.Expr: *→*`[F], tparama: `Type.Expr: *`[A], impl: Option[`Value.Expr: *`[T]], value: `Value.Var`[T]) extends Statement
    case class `1`[A, B](arg: `Value.Expr: *`[A], ret: `Type.Expr: *`[B], impl: Option[`Value.Expr: *`[B]], value: `Value.Var`[A => B]) extends Statement
    object `1`:
      case class `[_]`[T, A, B](tparam: `Type.Expr: *`[A], arg: `Value.Expr: *`[A], ret: `Type.Expr: *`[B], impl: Option[`Value.Expr: *`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_], _]`[F[_], T, A, B](tparamf: `Type.Expr: *→*`[F], tparama: `Type.Expr: *`[A], arg: `Value.Expr: *`[A], ret: `Type.Expr: *`[B], impl: Option[`Value.Expr: *`[T]], value: `Value.Var`[T]) extends Statement
  case class `field`[T](value: `Value.Var`[T] ) extends Statement
  case class `generator`[F[_], A](value: `Value.Var.Bound.Data`[F[A]]) extends Statement
  case class `object`[T](value: `Value.Var.Unbound.Data`[T], parent: Option[`Type.Expr: *`[T]], body: List[Statement]) extends Statement
  case class `package`(nme: Option[String], contents: List[Statement]) extends Statement
  case class `trait`[T](tpe: `Type.Var: *`[T], parent: Option[Type], body: List[Statement]) extends Statement
  object `trait`:
    case class `[_]`[T[_], A](tpe: `Type.Var: *→*`[T], tparam: `Type.Expr: *`[A], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_]]`[T[_[_]], F[_]](tpe: `Type.Var: (*→*)→*`[T], tparam: `Type.Expr: *→*`[F], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_], _]`[T[_[_], _], F[_], A](tpe: `Type.Var: (*→*)→*→*`[T], tparamf: `Type.Expr: *→*`[F], tparama: `Type.Expr: *`[A], parent: Option[Type], body: List[Statement]) extends Statement
  case class `type`[T](tpe: `Type.Var: *`[T]) extends Statement
  object `type`:
    case class `[_]`[F[_], A](tparam: `Type.Expr: *`[A], tpe: `Type.Var: *→*`[F]) extends Statement
    case class `[_]=>>`[F[_]](tpe: `Type.Var: *→*`[F]) extends Statement
    case class `[_[_]]`[F[_[_]], G[_]](tparam: `Type: *→*`[G], tpe: `Type.Var: (*→*)→*`[F]) extends Statement
    case class `[_[_], _]`[F[_[_], _], G[_], A](tparamf: `Type: *→*`[G], tparama: `Type.Expr: *`[A], tpe: `Type.Var: (*→*)→*→*`[F]) extends Statement
  case class `val`[T](value: `Value.Var`[T]) extends Statement