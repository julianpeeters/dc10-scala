package dc10.scala

import cats.data.NonEmptyList
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
        case d@ImportDefs(i, sp) => ImportDefs(d.terms, i + 1)(using sp)
        case d@ObjectDef(i, sp) => ObjectDef(d.obj, i + 1)(using sp)
        case d@PackageDef(i, sp) => PackageDef(d.pkg, i + 1)(using sp)
        case TypeExpr(tpe) => ???
        case ValueExpr(value) => ???
        case d@TypeDef.Alias(i, sp) => TypeDef.Alias(i + 1, d.tpe)(using sp)
        case d@TypeDef.Match(i, sp) => TypeDef.Match(i + 1, d.tpe, d.rhs)(using sp)
        case d@ValueDef.Def(i, sp) => ValueDef.Def(i + 1, d.value, d.arg, d.tpe, d.ret)(using sp)
        case d@ValueDef.Fld(i, sp) => ValueDef.Fld(i + 1, d.value)(using sp)
        case d@ValueDef.Gen(i, sp) => ValueDef.Gen(i + 1, d.value, d.impl)(using sp)
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

  sealed abstract case class ImportDefs(
    indent: Int,
    sp: SourcePos
  ) extends Statement:
    type Tpe
    def terms: List[Term]

  object ImportDefs:
    def apply[T](
      ts: List[Term],
      i: Int
    )(
      using sp: SourcePos
    ): ImportDefs =
      new ImportDefs(i, sp):
        type Tpe = T
        def terms: List[Term] = ts

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

  sealed trait TypeDef extends Statement:
    type Tpe
    type Zed
    def indent: Int
    def sp: SourcePos

  object TypeDef:

    abstract case class Alias[T](
      i: Int,
      s: SourcePos   
    ) extends TypeDef:
      type Tpe = T
      def indent: Int = i
      def sp: SourcePos = s
      def tpe: Term.TypeLevel.Var.UserDefinedType[T]


    object Alias:
      def apply[T](
        i: Int,
        t: Term.TypeLevel.Var.UserDefinedType[T]
      )(
        using sp: SourcePos
      ): TypeDef =
        new Alias[T](i, sp):
          def tpe: Term.TypeLevel.Var.UserDefinedType[T] = t

    abstract case class Match[T[_], A, B](
      i: Int,
      s: SourcePos   
    ) extends TypeDef:
      type Tpe = T[A]
      def indent: Int = i
      def sp: SourcePos = s
      def tpe: Term.TypeLevel.App.App1[T, A]
      def rhs: NonEmptyList[Term.TypeLevel.App.Infix[?, ?, ?]]

    object Match:
      def apply[T[_], A, B](
        i: Int,
        t: Term.TypeLevel.App.App1[T, A],
        f: NonEmptyList[Term.TypeLevel.App.Infix[?, ?, ?]]
      )(
        using sp: SourcePos
      ): TypeDef =
        new Match[T, A, B](i, sp):
          def tpe: Term.TypeLevel.App.App1[T, A] = t
          def rhs: NonEmptyList[Term.TypeLevel.App.Infix[?, ?, ?]] = f

  sealed trait ValueDef extends Statement:
    type Tpe
    def value: Term.ValueLevel.Var.UserDefinedValue[Tpe]
    def indent: Int
    def sp: SourcePos

  object ValueDef:

    abstract case class Def[T, A, B](
      i: Int,
      s: SourcePos
    ) extends ValueDef:
      type Tpe = T
      def arg: Term.ValueLevel[A]
      def ret: Option[Term.ValueLevel[B]]
      def tpe: Term.TypeLevel[B]
      def indent: Int = i
      def sp: SourcePos = s
    object Def:
      def apply[T, A, B](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T],
        a: Term.ValueLevel[A],
        t: Term.TypeLevel[B],
        r: Option[Term.ValueLevel[B]]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Def[T, A, B](i, sp):
          def arg: Term.ValueLevel[A] = a
          def ret: Option[Term.ValueLevel[B]] = r
          def tpe: Term.TypeLevel[B] = t
          def value: Term.ValueLevel.Var.UserDefinedValue[T] = v

    abstract case class Fld[T](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      def indent: Int = i
      def sp: SourcePos = s

    object Fld:
      def apply[T](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Fld[T](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T] = v
   
    
    abstract case class Gen[G[_], A](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = A
      def indent: Int = i
      def sp: SourcePos = s
      def impl: Term.ValueLevel[G[A]]

    object Gen:
      def apply[G[_], A](
        i: Int,
        u: Term.ValueLevel.Var.UserDefinedValue[A],
        v: Term.ValueLevel[G[A]]

      )(
        using sp: SourcePos
      ): ValueDef =
        new Gen[G, A](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[A] = u     
          def impl: Term.ValueLevel[G[A]] = v

    abstract case class Val[T](
      i: Int,
      s: SourcePos   
    ) extends ValueDef:
      type Tpe = T
      def indent: Int = i
      def sp: SourcePos = s

    object Val:
      def apply[T](
        i: Int,
        v: Term.ValueLevel.Var.UserDefinedValue[T]
      )(
        using sp: SourcePos
      ): ValueDef =
        new Val[T](i, sp):
          def value: Term.ValueLevel.Var.UserDefinedValue[T] = v
          def tpe: Term.TypeLevel[T] = v.tpe
   
  case class TypeExpr[T](tpe: Term.TypeLevel[T]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]

  case class ValueExpr[T](value: Term.ValueLevel[T]) extends Statement:
    def indent: Int = 0
    def sp: SourcePos = summon[SourcePos]