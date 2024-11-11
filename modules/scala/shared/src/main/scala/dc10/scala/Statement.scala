package dc10.scala

import cats.data.NonEmptyList
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Trait}
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}

sealed trait Statement
object Statement:

  case class `case class`[T](indent: Int, caseclass: CaseClass[T]) extends Statement
  case class `extension`(indent: Int, extension: Extension) extends Statement
  case class `object`[T](indent: Int, obj: Object[T]) extends Statement
  case class `package`(indent: Int, pkg: Package) extends Statement

  sealed trait TraitDef extends Statement
  object TraitDef:
    case class `trait`[T](indent: Int, `trait`: Trait.`*`[T]) extends TraitDef
    case class `trait[_]`[T[_], A](indent: Int, tparam: TypeLevel.`*`[A], `trait`: Trait.`*->*`[T]) extends TraitDef
    case class `trait[_[_]]`[T[_[_]], F[_]](indent: Int, tparam: TypeLevel.`*->*`[F], `trait`: Trait.`(*->*)->*`[T]) extends TraitDef
    case class `trait[_[_], _]`[T[_[_], _], F[_], A](indent: Int, tparamF: TypeLevel.`*->*`[F], tparamA: TypeLevel.`*`[A], `trait`: Trait.`(*->*)->*->*`[T]) extends TraitDef

  sealed trait TypeDef extends Statement
  object TypeDef:

    case class `Alias`[T](
      indent: Int,
      tpe: TypeLevel.Var.`UserDefinedType`[T]
    ) extends TypeDef

    case class `Alias[_]`[F[_], A](
      indent: Int,
      tparam: TypeLevel.`*`[A],
      tpe: TypeLevel.Var.`UserDefinedType[_]`[F]
    ) extends TypeDef

    case class `Alias[_]=>>`[F[_]](
      indent: Int,
      tpe: TypeLevel.Var.`UserDefinedType[_]`[F]
    ) extends TypeDef

    case class `Alias[_[_]]`[F[_[_]], G[_]](
      indent: Int,
      tparam: TypeLevel.`*->*`[G],
      tpe: TypeLevel.Var.`UserDefinedType[_[_]]`[F]
    ) extends TypeDef

    case class `Alias[_[_], _]`[F[_[_], _], G[_], A](
      indent: Int,
      tparamF: TypeLevel.`*->*`[G],
      tparamA: TypeLevel.`*`[A],
      tpe: TypeLevel.Var.`UserDefinedType[_[_], _]`[F]
    ) extends TypeDef
    
    case class `Match`[T[_], AA, A, B](
      indent: Int,
      tpe: TypeLevel.App.`App[_]`[T, A],
      rhs: NonEmptyList[TypeLevel.App.Infix[?, ?, ?]]
    ) extends TypeDef

  sealed trait ValueDef extends Statement
  object ValueDef:

    case class `def`[T, A, B](
      indent: Int,
      arg: Option[ValueLevel.`*`[A]],
      impl: Option[ValueLevel.`*`[B]],
      tpe: TypeLevel.`*`[B],
      value: ValueLevel.Var.`UserDefinedValue`[T],
    ) extends ValueDef

    case class `def[_]`[T, A](
      indent: Int,
      tparam: TypeLevel.`*`[A],
      impl: Option[ValueLevel.`*`[T]],
      value: ValueLevel.Var.`UserDefinedValue`[T],
    ) extends ValueDef

    case class `def[_[_]]`[F[_], A](
      indent: Int,
      tparam: TypeLevel.`*->*`[F],
      impl: Option[ValueLevel.`*`[A]],
      value: ValueLevel.Var.`UserDefinedValue`[A],
    ) extends ValueDef

    case class `def[_[_], _]`[F[_], A, T](
      indent: Int,
      tparamf: TypeLevel.`*->*`[F],
      tparama: TypeLevel.`*`[A],
      impl: Option[ValueLevel.`*`[T]],
      value: ValueLevel.Var.`UserDefinedValue`[T],
    ) extends ValueDef

    case class Fld[T](
      indent: Int,
      value: ValueLevel.Var.`UserDefinedValue`[T]
    ) extends ValueDef
    
    case class Gen[G[_], A](
      indent: Int,
      value: ValueLevel.Var.`UserDefinedValue`[A],
      impl: ValueLevel.`*`[G[A]]
    ) extends ValueDef
    
    case class `val`[K, T](
      indent: Int,
      value: ValueLevel.Var.`UserDefinedValue`[T],
      tpe: TypeLevel.`*`[T]
    ) extends ValueDef
  
  sealed trait TypeExpr extends Statement
  object TypeExpr:
    case class `Type`[T](tpe: TypeLevel.`*`[T]) extends TypeExpr
    case class `Type[_]`[F[_]](tpe: TypeLevel.`*->*`[F]) extends TypeExpr
    case class `Type[_[_]]`[F[_[_]]](tpe: TypeLevel.`(*->*)->*`[F]) extends TypeExpr
    case class `Type[_, _]`[F[_, _]](tpe: TypeLevel.`*->*->*`[F]) extends TypeExpr
    case class `Type[_[_], _]`[F[_[_], _]](tpe: TypeLevel.`(*->*)->*->*`[F]) extends TypeExpr

  sealed trait ValueExpr extends Statement
  object ValueExpr:
    case class Value[T](value: ValueLevel.`*`[T]) extends ValueExpr

  extension (s: Statement)
    def addIndent: Statement =
      s match
        case d@`case class`(i, c)                                       => `case class`(i + 1, c)
        case d@`extension`(i, e)                                        => `extension`(i + 1, e)
        case d@`object`(i, obj)                                         => `object`(i + 1, obj)
        case d@`package`(i, pkg)                                        => `package`(i + 1, d.pkg)
        case d@TraitDef.`trait`(i, t)                                   => TraitDef.`trait`(i + 1, d.`trait`)
        case d@TraitDef.`trait[_]`(i, p, t)                             => TraitDef.`trait[_]`(i + 1, p, t)
        case d@TraitDef.`trait[_[_]]`(i, p, t)                          => TraitDef.`trait[_[_]]`(i + 1, p, d.`trait`)
        case d@TraitDef.`trait[_[_], _]`(i, f, a, t)                    => TraitDef.`trait[_[_], _]`(i + 1, f, a, d.`trait`)
        case d@TypeDef.`Alias`(i, tpe)                                  => TypeDef.`Alias`(i + 1, d.tpe)
        case d@TypeDef.`Alias[_]=>>`(i, tpe)                            => TypeDef.`Alias[_]=>>`(i + 1, d.tpe)
        case d@TypeDef.`Alias[_]`(i, a, t)                              => TypeDef.`Alias[_]`(i + 1, d.tparam, d.tpe)
        case d@TypeDef.`Alias[_[_]]`(i, a, t)                           => TypeDef.`Alias[_[_]]`(i + 1, d.tparam, d.tpe)
        case d@TypeDef.`Alias[_[_], _]`(i, f, a, t)                     => TypeDef.`Alias[_[_], _]`(i + 1, f, a, t)
        case d@TypeDef.Match(i, _, _)                                   => TypeDef.Match(i + 1, d.tpe, d.rhs)
        case d@ValueDef.`def`(i, arg, ret, tpe, value)                  => ValueDef.`def`(i + 1, arg, ret, tpe, value)
        case d@ValueDef.`def[_]`(i, tparam, ret, value)                 => ValueDef.`def[_]`(i + 1, tparam, ret, value)
        case d@ValueDef.`def[_[_]]`(i, tparam, ret, value)              => ValueDef.`def[_[_]]`(i + 1, tparam, ret, value)
        case d@ValueDef.`def[_[_], _]`(i, tparamf, tparama, ret, value) => ValueDef.`def[_[_], _]`(i + 1, tparamf, tparama, ret, value)
        case d@ValueDef.Fld(i, v)                                       => ValueDef.Fld(i + 1, v)
        case d@ValueDef.Gen(i, value, impl)                             => ValueDef.Gen(i + 1, value, impl)
        case d@ValueDef.`val`(i, value, tpe)                            => ValueDef.`val`(i + 1, value, tpe)
        case d@TypeExpr.`Type`(_)                                       => d
        case d@TypeExpr.`Type[_]`(_)                                    => d
        case d@TypeExpr.`Type[_[_]]`(_)                                 => d
        case d@TypeExpr.`Type[_, _]`(_)                                 => d
        case d@TypeExpr.`Type[_[_], _]`(_)                              => d
        case d@ValueExpr.`Value`(_)                                     => d
