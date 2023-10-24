package dc10.scala

import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}
import org.tpolecat.sourcepos.SourcePos

sealed trait Statement:
  def indent: Int
  def sp: SourcePos
  
object Statement:

  extension (s: Statement)
    def addIndent: Statement =
      s match
        case d@CaseClassDef(i, sp) => CaseClassDef(d.caseclass, i + 1)(using sp)
        case d@ExtensionDef(i, sp) => ExtensionDef(d.extension, i + 1)(using sp)
        case d@ObjectDef(i, sp) => ObjectDef(d.obj, i + 1)(using sp)
        case d@PackageDef(i, sp) => PackageDef(d.pkg, i + 1)(using sp)
        case TypeExpr(tpe) => ???
        case ValueExpr(value) => ???
        case d@ValueDef.Def(i, sp) => ValueDef.Def(i + 1, d.value, d.arg, d.tpe, d.ret)(using sp)
        case d@ValueDef.Fld(i, sp) => ValueDef.Fld(i + 1, d.value)(using sp)
        case d@ValueDef.Val(i, sp) => ValueDef.Val(i + 1, d.value)(using sp)

  sealed abstract case class CaseClassDef(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    type Tpe
    def caseclass: CaseClass[Tpe]

  object CaseClassDef:
    def apply[T](
      v: CaseClass[T],
      i: Int
    )(
      using sp: SourcePos
    ): CaseClassDef =
      new CaseClassDef(i, sp):
        type Tpe = T
        def caseclass: CaseClass[T] = v

  
  sealed abstract case class ExtensionDef(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    def extension: Extension

  object ExtensionDef:
    def apply(
      v: Extension,
      i: Int
    )(
      using sp: SourcePos
    ): ExtensionDef =
      new ExtensionDef(i, sp):
        def extension: Extension = v

  sealed abstract case class ObjectDef(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    type Tpe
    def obj: Object[Tpe]

  object ObjectDef:
    def apply[T](
      o: Object[T],
      i: Int
    )(
      using sp: SourcePos
    ): ObjectDef =
      new ObjectDef(i, sp):
        type Tpe = T
        def obj: Object[T] = o

  sealed abstract case class PackageDef(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    def pkg: Package

  object PackageDef:
    def apply[T](
      p: Package,
      i: Int,
    )(
      using sp: SourcePos
    ): PackageDef =
      new PackageDef(i, sp):
        def pkg: Package = p

  sealed trait ValueDef extends Statement:
    type Tpe
    type Nom <: Term.ValueLevel[Tpe, Nom]
    def value: Term.ValueLevel.Var.UserDefinedValue[Tpe, Nom]
    def indent: Int
    def sp: SourcePos

  object ValueDef:

    abstract case class Def[T, A, B, X <: Term.ValueLevel[T, X]](
      i: Int,
      s: SourcePos
    ) extends ValueDef:
      type Tpe = T
      type Nom = X
      def arg: Term.Value[A]
      def ret: Option[Term.Value[B]]
      def tpe: Term.Type[B]
      def indent: Int = i
      def sp: SourcePos = s
    object Def:
      def apply[T, A, B, X <: Term.ValueLevel[T, X]](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, X],
        a: Term.Value[A],
        t: Term.Type[B],
        r: Option[Term.Value[B]]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Def[T, A, B, X](i, sp):
          def arg: Term.Value[A] = a
          def ret: Option[Term.Value[B]] = r
          def tpe: Term.Type[B] = t
          def value: Term.ValueLevel.Var.UserDefinedValue[T, X] = v


    abstract case class Fld[T, X <: Term.ValueLevel[T, X]](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      type Nom = X
      // def sp: SourcePos = s
      def indent: Int = i
      def sp: SourcePos = s

    object Fld:
      def apply[T, A <: Term.ValueLevel[T, A]](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, A]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Fld[T, A](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T, A] = v
   

    abstract case class Val[T, X <: Term.ValueLevel[T, X]](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      type Nom = X
      // def sp: SourcePos = s
      def indent: Int = i
      def sp: SourcePos = s

    object Val:
      def apply[T, A <: Term.ValueLevel[T, A]](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, A]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Val[T, A](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T, A] = v
   
  case class TypeExpr[T](tpe: Term.Type[T]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]

  case class ValueExpr[T](value: Term.Value[T]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]