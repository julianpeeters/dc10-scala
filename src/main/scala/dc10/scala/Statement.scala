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
        case d@TypeDef.Alias(i, sp) => TypeDef.Alias(i + 1, d.tpe)(using sp)
        case d@ValueDef.Def(i, sp) => ValueDef.Def(i + 1, d.value, d.arg, d.tpe, d.ret)(using sp)
        case d@ValueDef.Fld(i, sp) => ValueDef.Fld(i + 1, d.value)(using sp)
        case d@ValueDef.Val(i, sp) => ValueDef.Val(i + 1, d.value)(using sp)

  sealed abstract case class CaseClassDef(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    type Tpe
    type Zed
    def caseclass: CaseClass[Tpe, Zed]

  object CaseClassDef:
    def apply[T, Z](
      v: CaseClass[T, Z],
      i: Int
    )(
      using sp: SourcePos
    ): CaseClassDef =
      new CaseClassDef(i, sp):
        type Tpe = T
        type Zed = Z
        def caseclass: CaseClass[T, Z] = v

  
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
    def obj: Object[Tpe, Unit]

  object ObjectDef:
    def apply[T](
      o: Object[T, Unit],
      i: Int
    )(
      using sp: SourcePos
    ): ObjectDef =
      new ObjectDef(i, sp):
        type Tpe = T
        def obj: Object[T, Unit] = o

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

  sealed trait TypeDef extends Statement:
    type Tpe
    type Zed
    def tpe: Term.TypeLevel.Var.UserDefinedType[Tpe, Zed]
    def indent: Int
    def sp: SourcePos

  object TypeDef:

    abstract case class Alias[T, Z](
      i: Int,
      s: SourcePos   
    ) extends TypeDef:
      type Tpe = T
      type Zed = Z
      def indent: Int = i
      def sp: SourcePos = s

    object Alias:
      def apply[T, Z](
        i: Int,
        t: Term.TypeLevel.Var.UserDefinedType[T, Z]
      )(
        using sp: SourcePos
      ): TypeDef =
        new Alias[T, Z](i, sp):
          def tpe: Term.TypeLevel.Var.UserDefinedType[T, Z] = t
          // def tpe: Term.TypeLevel[T, Z] = v.tpe
   


  sealed trait ValueDef extends Statement:
    type Tpe
    type Zed
    def value: Term.ValueLevel.Var.UserDefinedValue[Tpe, Zed]
    def indent: Int
    def sp: SourcePos

  object ValueDef:

    abstract case class Def[T, A, B, Z](
      i: Int,
      s: SourcePos
    ) extends ValueDef:
      type Tpe = T
      type Zed = Z
      def arg: Term.ValueLevel[A, Z]
      def ret: Option[Term.ValueLevel[B, Z]]
      def tpe: Term.TypeLevel[B, Z]
      def indent: Int = i
      def sp: SourcePos = s
    object Def:
      def apply[T, A, B, Z](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, Z],
        a: Term.ValueLevel[A, Z],
        t: Term.TypeLevel[B, Z],
        r: Option[Term.ValueLevel[B, Z]]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Def[T, A, B, Z](i, sp):
          def arg: Term.ValueLevel[A, Z] = a
          def ret: Option[Term.ValueLevel[B, Z]] = r
          def tpe: Term.TypeLevel[B, Z] = t
          def value: Term.ValueLevel.Var.UserDefinedValue[T, Z] = v


    abstract case class Fld[T, Z](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      type Zed = Z
      def indent: Int = i
      def sp: SourcePos = s

    object Fld:
      def apply[T, Z](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, Z]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Fld[T, Z](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T, Z] = v
   

    abstract case class Val[T, Z](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      type Zed = Z
      def indent: Int = i
      def sp: SourcePos = s

    object Val:
      def apply[T, Z](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T, Z]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Val[T, Z](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T, Z] = v
          def tpe: Term.TypeLevel[T, Z] = v.tpe
   
  case class TypeExpr[T, Z](tpe: Term.TypeLevel[T, Z]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]

  case class ValueExpr[T, Z](value: Term.ValueLevel[T, Z]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]