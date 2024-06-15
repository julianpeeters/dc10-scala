package dc10.scala

import cats.data.NonEmptyList
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Trait}
import dc10.scala.Symbol.Term.{TypeLevel, ValueLevel}
import org.tpolecat.sourcepos.SourcePos

sealed trait Statement
object Statement:

  case class `case class`[T](indent: Int, sp: SourcePos, caseclass: CaseClass[T]) extends Statement
  case class `extension`(indent: Int, sp: SourcePos, extension: Extension) extends Statement
  case class `object`[T](indent: Int, sp: SourcePos, obj: Object[T]) extends Statement
  case class `package`(indent: Int, sp: SourcePos, pkg: Package) extends Statement

  sealed trait TraitDef extends Statement
  object TraitDef:
    case class `trait`[T](indent: Int, sp: SourcePos, `trait`: Trait.`*`[T]) extends TraitDef
    case class `trait[_]`[T[_], A](indent: Int, sp: SourcePos, tparam: TypeLevel.`*`[A], `trait`: Trait.`*->*`[T]) extends TraitDef
    case class `trait[_[_]]`[T[_[_]], F[_]](indent: Int, sp: SourcePos, tparam: TypeLevel.`*->*`[F], `trait`: Trait.`(*->*)->*`[T]) extends TraitDef
    case class `trait[_[_], _]`[T[_[_], _], F[_], A](indent: Int, sp: SourcePos, tparamF: TypeLevel.`*->*`[F], tparamA: TypeLevel.`*`[A], `trait`: Trait.`(*->*)->*->*`[T]) extends TraitDef

  sealed trait TypeDef extends Statement
  object TypeDef:

    case class `Alias`[T](
      indent: Int,
      sp: SourcePos,
      tpe: TypeLevel.Var.`UserDefinedType`[T]
    ) extends TypeDef

    case class `Alias[_]`[F[_], A](
      indent: Int,
      sp: SourcePos,
      tparam: TypeLevel.`*`[A],
      tpe: TypeLevel.Var.`UserDefinedType[_]`[F]
    ) extends TypeDef

    case class `Alias[_]=>>`[F[_]](
      indent: Int,
      sp: SourcePos,
      tpe: TypeLevel.Var.`UserDefinedType[_]`[F]
    ) extends TypeDef

    case class `Alias[_[_]]`[F[_[_]], G[_]](
      indent: Int,
      sp: SourcePos,
      tparam: TypeLevel.`*->*`[G],
      tpe: TypeLevel.Var.`UserDefinedType[_[_]]`[F]
    ) extends TypeDef

    case class `Alias[_[_], _]`[F[_[_], _], G[_], A](
      indent: Int, sp: SourcePos,
      tparamF: TypeLevel.`*->*`[G],
      tparamA: TypeLevel.`*`[A],
      tpe: TypeLevel.Var.`UserDefinedType[_[_], _]`[F]
    ) extends TypeDef
    
    case class `Match`[T[_], AA, A, B](
      indent: Int,
      sp: SourcePos,
      tpe: TypeLevel.App.`App[_]`[T, A],
      rhs: NonEmptyList[TypeLevel.App.Infix[?, ?, ?]]
    ) extends TypeDef

  sealed trait ValueDef extends Statement
  object ValueDef:

    case class `def`[T, A, B](
      indent: Int,
      sp: SourcePos,
      arg: Option[ValueLevel.`*`[A]],
      impl: Option[ValueLevel.`*`[B]],
      tpe: TypeLevel.`*`[B],
      value: ValueLevel.Var.`UserDefinedValue`[T],
    ) extends ValueDef

    case class `def[_]`[T, A](
      indent: Int,
      sp: SourcePos,
      tparam: TypeLevel.`*`[A],
      impl: Option[ValueLevel.`*`[T]],
      value: ValueLevel.Var.`UserDefinedValue`[T],
    ) extends ValueDef

    case class `def[_[_]]`[F[_], A](
      indent: Int,
      sp: SourcePos,
      tparam: TypeLevel.`*->*`[F],
      impl: Option[ValueLevel.`*`[A]],
      value: ValueLevel.Var.`UserDefinedValue`[A],
    ) extends ValueDef

    case class Fld[T](
      indent: Int,
      sp: SourcePos,
      value: ValueLevel.Var.`UserDefinedValue`[T]
    ) extends ValueDef
    
    case class Gen[G[_], A](
      indent: Int,
      sp: SourcePos,
      value: ValueLevel.Var.`UserDefinedValue`[A],
      impl: ValueLevel.`*`[G[A]]
    ) extends ValueDef
    
    case class `val`[K, T](
      indent: Int,
      sp: SourcePos,
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
        case d@`case class`(i, sp, c)                               => `case class`(i + 1, sp, c)
        case d@`extension`(i, sp, e)                                => `extension`(i + 1, sp, e)
        case d@`object`(i, sp, obj)                                 => `object`(i + 1, sp, obj)
        case d@`package`(i, sp, pkg)                               => `package`(i + 1, sp, d.pkg)
        case d@TraitDef.`trait`(i, sp, t)                           => TraitDef.`trait`(i + 1, sp, d.`trait`)
        case d@TraitDef.`trait[_]`(i, sp, p, t)                     => TraitDef.`trait[_]`(i + 1, sp, p, t)
        case d@TraitDef.`trait[_[_]]`(i, sp, p, t)                  => TraitDef.`trait[_[_]]`(i + 1, sp, p, d.`trait`)
        case d@TraitDef.`trait[_[_], _]`(i, sp, f, a, t)            => TraitDef.`trait[_[_], _]`(i + 1, sp, f, a, d.`trait`)
        case d@TypeDef.`Alias`(i, sp, tpe)                          => TypeDef.`Alias`(i + 1, sp, d.tpe)
        case d@TypeDef.`Alias[_]=>>`(i, sp, tpe)                    => TypeDef.`Alias[_]=>>`(i + 1, sp, d.tpe)
        case d@TypeDef.`Alias[_]`(i, sp, a, t)                      => TypeDef.`Alias[_]`(i + 1, sp, d.tparam, d.tpe)
        case d@TypeDef.`Alias[_[_]]`(i, sp, a, t)                   => TypeDef.`Alias[_[_]]`(i + 1, sp, d.tparam, d.tpe)
        case d@TypeDef.`Alias[_[_], _]`(i, sp, f, a, t)             => TypeDef.`Alias[_[_], _]`(i + 1, sp, f, a, t)
        case d@TypeDef.Match(i, sp, _, _)                           => TypeDef.Match(i + 1, sp, d.tpe, d.rhs)
        case d@ValueDef.`def`(i, sp, arg, ret, tpe, value)          => ValueDef.`def`(i + 1, sp, arg, ret, tpe, value)
        case d@ValueDef.`def[_]`(i, sp, tparam, ret, value)         => ValueDef.`def[_]`(i + 1, sp, tparam, ret, value)
        case d@ValueDef.`def[_[_]]`(i, sp, tparam, ret, value)      => ValueDef.`def[_[_]]`(i + 1, sp, tparam, ret, value)
        case d@ValueDef.Fld(i, sp, v)                               => ValueDef.Fld(i + 1, sp, v)
        case d@ValueDef.Gen(i, sp, value, impl)                     => ValueDef.Gen(i + 1, sp, value, impl)
        case d@ValueDef.`val`(i, sp, value, tpe)                      => ValueDef.`val`(i + 1, sp, value, tpe)
        case d@TypeExpr.`Type`(_)                                   => d
        case d@TypeExpr.`Type[_]`(_)                                => d
        case d@TypeExpr.`Type[_[_]]`(_)                             => d
        case d@TypeExpr.`Type[_, _]`(_)                             => d
        case d@TypeExpr.`Type[_[_], _]`(_)                          => d
        case d@ValueExpr.`Value`(_)                                 => d
