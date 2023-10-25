package dc10.scala.version

import dc10.compile.Renderer
import dc10.scala.Statement
import dc10.scala.Statement.{CaseClassDef, ExtensionDef, PackageDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}
import dc10.scala.Symbol.Term.ValueLevel.{App, Lam}
import dc10.scala.Error

given `3.3.1`: Renderer["scala-3.3.1", Error, List[Statement]] =
  new Renderer["scala-3.3.1", Error, List[Statement]]:

    override def render(input: List[Statement]): String = input.map(stmt => stmt match
      case d@CaseClassDef(_, _)        => indent(d.indent) ++ renderCaseClass(d.caseclass)
      case d@ExtensionDef(_, _)        => indent(d.indent) ++ renderExtension(d.extension)
      case d@Statement.ObjectDef(_, _) => indent(d.indent) ++ renderObject(d.obj)
      case d@PackageDef(_, _)          => indent(d.indent) ++ renderPackage(d.pkg)
      case d@ValueDef.Def(_, _)        => indent(d.indent) ++ renderValueDef(d)
      case d@ValueDef.Fld(_, _)        => indent(d.indent) ++ renderValueDef(d)
      case d@ValueDef.Val(_, _)        => indent(d.indent) ++ renderValueDef(d)
      case e@TypeExpr(t)               => indent(e.indent) ++ renderType(t.tail.value)
      case e@ValueExpr(v)              => indent(e.indent) ++ renderValue(v.tail.value)
    ).mkString("\n")

    override def renderErrors(errors: List[Error]): String =
      errors.map(_.toString()).mkString("\n")

    override def version: "scala-3.3.1" =
      "scala-3.3.1"

    private def indent(i: Int): String =
      "  ".repeat(i)

    private def renderCaseClass[T](cls: CaseClass[T]) =
      s"case class ${cls.nme}(${render(cls.fields).mkString})"

    private def renderExtension(ext: Extension) =
      s"extension (${render(List(ext.field)).mkString})\n  ${render(ext.body)}\n"

    private def renderObject[T](obj: Object[T]): String =
      obj.par.fold(s"object ${obj.nme}:\n\n${render(obj.body)}")(p =>
        s"object ${obj.nme} extends ${renderType(p.tail.value)}:\n\n${render(obj.body)}"
      )

    private def renderPackage(pkg: Package): String =
      pkg match
        case Package.Basic(nme, pkgdef) => s"package ${nme}\n\n${renderPackage(pkgdef.pkg)}"
        case Package.Empty(ms) => render(ms)
      
    private def renderType[T, X](tpe: Term.TypeLevel[T, X]): String =
      tpe match
        // application
        case Term.TypeLevel.App1(qnt, tfun, targ) => s"${renderType(tfun.tail.value)}[${renderType(targ.tail.value)}]"
        case Term.TypeLevel.App2(qnt, tfun, ta, tb) => s"${renderType(ta.tail.value)} ${renderType(tfun.tail.value)} ${renderType(tb.tail.value)}"
        case Term.TypeLevel.App3(qnt, tfun, ta1, ta2, tb) => s"${renderType(ta1.tail.value)} ${renderType(tfun.tail.value)} ${renderType(tb.tail.value)}"
        // primitive
        case Term.TypeLevel.Var.BooleanType(_) => "Boolean"
        case Term.TypeLevel.Var.IntType(_) => "Int"
        case Term.TypeLevel.Var.StringType(_) => "String"
        // complex
        case Term.TypeLevel.Var.Function1Type(_) => "=>"
        case Term.TypeLevel.Var.Function2Type(_) => "=>"
        case Term.TypeLevel.Var.ListType(_) => "List"
        case Term.TypeLevel.Var.OptionType(_) => "Option"
        case Term.TypeLevel.Var.OptionType.SomeType(_) => "Some"
        case Term.TypeLevel.Var.UserDefinedType(q, s, i) => s

    private def renderValue[T, X](value: Term.ValueLevel[T, X]): String =
      value match 
        // application
        case Term.ValueLevel.App.App1(q, f, a, t) => s"${renderValue(f.tail.value)}(${renderValue(a.tail.value)})"
        case Term.ValueLevel.App.AppCtor1(q, t, a) => s"${renderType(t.tail.value)}(${renderValue(a.tail.value)})"
        case Term.ValueLevel.App.AppVargs(q, f, as*) => s"${renderValue(f.tail.value)}(${as.map(a => renderValue(a.tail.value)).mkString(", ")})"
        case Term.ValueLevel.App.Dot1(q, f, a, b) => s"${renderValue(a.tail.value)}.${renderValue(f.tail.value)}(${renderValue(b.tail.value)})"
        // function
        case Term.ValueLevel.Lam.Lam1(q, a, b) => s"${renderValue(a.tail.value)} => ${renderValue(b.tail.value)}"
        case Term.ValueLevel.Lam.Lam2(q, a1, a2, b) => s"(${renderValue(a1.tail.value)}, ${renderValue(a2.tail.value)}) => ${renderValue(b.tail.value)}"
        // primitive
        case Term.ValueLevel.Var.BooleanLiteral(q, b) => s"$b"
        case Term.ValueLevel.Var.IntLiteral(q, i) => s"$i"
        case Term.ValueLevel.Var.StringLiteral(q, s) => s"\"${s}\""
        // complex
        case Term.ValueLevel.Var.ListCtor(q) => s"List"
        case Term.ValueLevel.Var.OptionCtor(q) => s"Option"
        case Term.ValueLevel.Var.OptionCtor.SomeCtor(q) => s"Some"
        case Term.ValueLevel.Var.Println(q, s) => s"IO.println(${renderValue(s.tail.value)})"
        case Term.ValueLevel.Var.UserDefinedValue(q, s, t, i) => s

    private def renderValueDef(valueDef: ValueDef): String =
      valueDef match
        case d@ValueDef.Def(_, _) =>
          d.ret.fold(
            s"def ${d.value.nme}(${renderValue(d.arg.tail.value)}: ${renderType(d.arg.tail.value.tpe.tail.value)}): ${renderType(d.tpe.tail.value)}"
          )(
            i => s"def ${d.value.nme}(${renderValue(d.arg.tail.value)}: ${renderType(d.arg.tail.value.tpe.tail.value)}): ${renderType(d.tpe.tail.value)} = ${renderValue(i.tail.value)}"
          )
        case d@ValueDef.Fld(_, _)  =>
          d.value.impl.fold(
            s"${d.value.nme}: ${renderType(d.value.tpe.tail.value)}"
          )(
            i =>
              s"${d.value.nme}: ${renderType(d.value.tpe.tail.value)} = ${renderValue(i.tail.value)}"
          )
        case d@ValueDef.Val(_, _)  =>
          d.value.impl.fold(
            s"val ${d.value.nme}: ${renderType(d.value.tpe.tail.value)}"
          )(
            i =>
              s"val ${d.value.nme}: ${renderType(d.value.tpe.tail.value)} = ${renderValue(i.tail.value)}"
          )