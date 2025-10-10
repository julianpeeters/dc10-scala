package dc10.scala

sealed trait Statement
object Statement:
  
  case class `case`[T](lambda: `Value: x`[T]) extends Statement
  case class `case class`[T](tpe: `Type.Var: x`[T], fields: List[Statement], body: List[Statement]) extends Statement
  case class `extension`[T](field: `Value.Var.Unbound.Data`[T], body: List[Statement]) extends Statement
  object `def`:
    case class `0`[A](value: `Value.Var`[A]) extends Statement
    object `0`:
      case class `[_]`[T, A](tparam: `Type: x`[A], impl: Option[`Value: x`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_]]`[F[_], A](tparam: `Type: x→x`[F], impl: Option[`Value: x`[A]], value: `Value.Var`[A]) extends Statement
      case class `[_[_], _]`[F[_], A, T](tparamf: `Type: x→x`[F], tparama: `Type: x`[A], impl: Option[`Value: x`[T]], value: `Value.Var`[T]) extends Statement
    case class `1`[A, B](arg: `Value: x`[A], ret: `Type: x`[B], impl: Option[`Value: x`[B]], value: `Value.Var`[A => B]) extends Statement
    object `1`:
      case class `[_]`[T, A, B](tparam: `Type: x`[A], arg: `Value: x`[A], ret: `Type: x`[B], impl: Option[`Value: x`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_], _]`[F[_], T, A, B](tparamf: `Type: x→x`[F], tparama: `Type: x`[A], arg: `Value: x`[A], ret: `Type: x`[B], impl: Option[`Value: x`[T]], value: `Value.Var`[T]) extends Statement
  case class `field`[T](value: `Value.Var`[T] ) extends Statement
  case class `generator`[F[_], A](value: `Value.Var.Bound.Data`[F[A]]) extends Statement
  case class `object`[T](value: `Value.Var.Unbound.Data`[T], parent: Option[`Type: x`[T]], body: List[Statement]) extends Statement
  case class `package`(nme: Option[String], contents: List[Statement]) extends Statement
  case class `trait`[T](tpe: `Type.Var: x`[T], parent: Option[Type], body: List[Statement]) extends Statement
  object `trait`:
    case class `[_]`[T[_], A](tpe: `Type.Var: x→x`[T], tparam: `Type: x`[A], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_]]`[T[_[_]], F[_]](tpe: `Type.Var: (x→x)→x`[T], tparam: `Type: x→x`[F], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_], _]`[T[_[_], _], F[_], A](tpe: `Type.Var: (x→x)→x→x`[T], tparamf: `Type: x→x`[F], tparama: `Type: x`[A], parent: Option[Type], body: List[Statement]) extends Statement
  case class `type`[T](tpe: `Type.Var: x`[T]) extends Statement
  object `type`:
    case class `[_]`[F[_], A](tparam: `Type: x`[A], tpe: `Type.Var: x→x`[F]) extends Statement
    case class `[_]=>>`[F[_]](tpe: `Type.Var: x→x`[F]) extends Statement
    case class `[_[_]]`[F[_[_]], G[_]](tparam: `Type: x→x`[G], tpe: `Type.Var: (x→x)→x`[F]) extends Statement
    case class `[_[_], _]`[F[_[_], _], G[_], A](tparamf: `Type: x→x`[G], tparama: `Type: x`[A], tpe: `Type.Var: (x→x)→x→x`[F]) extends Statement
  case class `val`[T](value: `Value.Var`[T]) extends Statement