package dc10.scala

sealed trait Statement
object Statement:
  
  case class `case class`[T](tpe: `Type.Var`[T], fields: List[Statement], body: List[Statement]) extends Statement
  case class `extension`[T](field: `Value.VarA`[T], body: List[Statement]) extends Statement
  object `def`:
    case class `0`[A](value: `Value.Var`[A]) extends Statement
    object `0`:
      case class `[_]`[T, A](tparam: `Type.*`[A], impl: Option[`Value.*`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_]]`[F[_], A](tparam: `Type.*->*`[F], impl: Option[`Value.*`[A]], value: `Value.Var`[A]) extends Statement
      case class `[_[_], _]`[F[_], A, T](tparamf: `Type.*->*`[F], tparama: `Type.*`[A], impl: Option[`Value.*`[T]], value: `Value.Var`[T]) extends Statement
    case class `1`[A, B](arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[B]], value: `Value.Var`[A => B]) extends Statement
    object `1`:
      case class `[_]`[T, A, B](tparam: `Type.*`[A], arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[T]], value: `Value.Var`[T]) extends Statement
      case class `[_[_], _]`[F[_], T, A, B](tparamf: `Type.*->*`[F], tparama: `Type.*`[A], arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[T]], value: `Value.Var`[T]) extends Statement
  case class `field`[T](value: `Value.Var`[T] ) extends Statement
  case class `generator`[F[_], A](value: `Value.VarC`[F[A]]) extends Statement
  case class `object`[T](value: `Value.VarA`[T], parent: Option[`Type.*`[T]], body: List[Statement]) extends Statement
  case class `package`(nme: Option[String], contents: List[Statement]) extends Statement
  case class `trait`[T](tpe: `Type.Var`[T], parent: Option[Type], body: List[Statement]) extends Statement
  object `trait`:
    case class `[_]`[T[_], A](tpe: `Type.Var[_]`[T], tparam: `Type.*`[A], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_]]`[T[_[_]], F[_]](tpe: `Type.Var[_[_]]`[T], tparam: `Type.*->*`[F], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_], _]`[T[_[_], _], F[_], A](tpe: `Type.Var[_[_], _]`[T], tparamf: `Type.*->*`[F], tparama: `Type.*`[A], parent: Option[Type], body: List[Statement]) extends Statement
  case class `type`[T](tpe: `Type.Var`[T]) extends Statement
  object `type`:
    case class `[_]`[F[_], A](tparam: `Type.*`[A], tpe: `Type.Var[_]`[F]) extends Statement
    case class `[_]=>>`[F[_]](tpe: `Type.Var[_]`[F]) extends Statement
    case class `[_[_]]`[F[_[_]], G[_]](tparam: `Type.*->*`[G], tpe: `Type.Var[_[_]]`[F]) extends Statement
    case class `[_[_], _]`[F[_[_], _], G[_], A](tparamf: `Type.*->*`[G], tparama: `Type.*`[A], tpe: `Type.Var[_[_], _]`[F]) extends Statement
  case class `val`[T](value: `Value.Var`[T]) extends Statement