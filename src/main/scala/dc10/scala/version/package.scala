package dc10.scala.version

import dc10.compile.Renderer
import dc10.scala.Statement
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}
import dc10.scala.Symbol.Term.ValueLevel.{App, Lam}
import dc10.scala.Error
import dc10.scala.Statement.TypeDef.Alias

given `3.3.1`: Renderer["scala-3.3.1", Error, List[Statement]] =
  new Renderer["scala-3.3.1", Error, List[Statement]]:

    override def render(input: List[Statement]): String =
      
      input.map(stmt => stmt match
        case d@Statement.CaseClassDef(_, _)        => indent(d.indent) ++ renderCaseClass(d.caseclass)
        case d@Statement.ExtensionDef(_, _)        => indent(d.indent) ++ renderExtension(d.extension)
        case d@Statement.ObjectDef(_, _)           => indent(d.indent) ++ renderObject(d.obj)
        case d@Statement.PackageDef(_, _)          => indent(d.indent) ++ renderPackage(d.pkg)
        case d@Statement.TypeDef.Alias(_, _)       => indent(d.indent) ++ renderTypeDef(d)
        case d@Statement.ValueDef.Def(_, _)        => indent(d.indent) ++ renderValueDef(d)
        case d@Statement.ValueDef.Fld(_, _)        => indent(d.indent) ++ renderFieldDef(d, input)
        case d@Statement.ValueDef.Val(_, _)        => indent(d.indent) ++ renderValueDef(d)
        case e@Statement.TypeExpr(t)               => indent(e.indent) ++ renderType(t)
        case e@Statement.ValueExpr(v)              => indent(e.indent) ++ renderValue(v)
      ).mkString("\n")

    override def renderErrors(errors: List[Error]): String =
      errors.map(_.toString()).mkString("\n")

    override def version: "scala-3.3.1" =
      "scala-3.3.1"

    private def indent(i: Int): String =
      "  ".repeat(i)

    private def renderCaseClass[Z, T](cls: CaseClass[Z, T]) =
      s"case class ${cls.nme}(${
        if cls.fields.length <= 1
        then render(cls.fields)
        else "\n" ++ render(cls.fields) ++ "\n"
      })"

    private def renderExtension(ext: Extension) =
      s"extension (${render(List(ext.field)).mkString})\n  ${render(ext.body)}\n"

    private def renderFieldDef[T, Z](d: Statement.ValueDef.Fld[T, Z], input: List[Statement]): String =
      if input.length <= 1
      then renderValueDef(d)
      else indent(d.indent + 1) ++ renderValueDef(d) ++ ","

    private def renderObject[Z, T](obj: Object[Z, T]): String =
      obj.par.fold(s"object ${obj.nme}:\n\n${render(obj.body)}")(p =>
        s"object ${obj.nme} extends ${renderType(p)}:\n\n${render(obj.body)}"
      )

    private def renderPackage(pkg: Package): String =
      pkg match
        case Package.Basic(nme, pkgdef) => s"package ${nme}\n\n${renderPackage(pkgdef.pkg)}"
        case Package.Empty(ms) => render(ms)
      
    private def renderType[Z, T, X](tpe: Term.TypeLevel[T, X]): String =
      tpe match
        // application
        case Term.TypeLevel.App.App1(qnt, tfun, targ, z) => s"${renderType(tfun)}[${renderType(targ)}]"
        case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, z) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
        case Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, z) => s"${renderType(ta1)} ${renderType(tfun)} ${renderType(tb)}"
        // primitive
        case Term.TypeLevel.Var.BooleanType(_, z) => "Boolean"
        case Term.TypeLevel.Var.IntType(_, z) => "Int"
        case Term.TypeLevel.Var.StringType(_, z) => "String"
        // complex
        case Term.TypeLevel.Lam.Function1Type(_, z) => "=>"
        case Term.TypeLevel.Lam.Function2Type(_, z) => "=>"
        case Term.TypeLevel.Var.ListType(_, z) => "List"
        case Term.TypeLevel.Var.OptionType(_, z) => "Option"
        case Term.TypeLevel.Var.SomeType(_, z) => "Some"
        case Term.TypeLevel.Var.UserDefinedType(q, s, i, z) => s

    private def renderTypeDef(typeDef: Statement.TypeDef): String =
      typeDef match
        case d@Alias(i, s) => d.tpe.impl.fold(
          s"type ${d.tpe.nme}"
        )(i =>
          s"type ${d.tpe.nme} = ${renderType(i)}"
        )

    private def renderValue[Z, T, X](value: Term.ValueLevel[T, Z]): String =
      value match 
        // application
        case Term.ValueLevel.App.App1(q, f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppCtor1(q, t, a) => s"${renderType(t)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppCtor2(q, t, a, b) => s"${renderType(t)}(${renderValue(a)}, ${renderValue(b)})"
        case Term.ValueLevel.App.AppPure(q, f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppVargs(q, f, t, as*) => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
        case Term.ValueLevel.App.Dot1(q, f, a, b, t) => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
        case Term.ValueLevel.App.Dotless(q, f, a, b, t) => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
        // function
        case Term.ValueLevel.Lam.Lam1(q, a, b, t) => s"${renderValue(a)} => ${renderValue(b)}"
        case Term.ValueLevel.Lam.Lam2(q, a1, a2, b, t) => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(b)}"
        // primitive
        case Term.ValueLevel.Var.BooleanLiteral(q, tpe, b) => s"$b"
        case Term.ValueLevel.Var.IntLiteral(q, tpe, i) => s"$i"
        case Term.ValueLevel.Var.StringLiteral(q, tpe, s) => s"\"${s}\""
        // complex
        case Term.ValueLevel.Var.ListCtor(q, tpe) => s"List"
        case Term.ValueLevel.Var.OptionCtor(q, tpe) => s"Option"
        case Term.ValueLevel.Var.SomeCtor(q, tpe) => s"Some"
        case Term.ValueLevel.Var.UserDefinedValue(q, s, t, i) => s

    private def renderValueDef(valueDef: Statement.ValueDef): String =
      valueDef match
        case d@Statement.ValueDef.Def(_, _) =>
          d.ret.fold(
            s"def ${d.value.nme}(${renderValue(d.arg)}: ${renderType(d.arg.tpe)}): ${renderType(d.tpe)}"
          )(
            i => s"def ${d.value.nme}(${renderValue(d.arg)}: ${renderType(d.arg.tpe)}): ${renderType(d.tpe)} = ${renderValue(i)}"
          )
        case d@Statement.ValueDef.Fld(_, _)  =>
          d.value.impl.fold(
            s"${d.value.nme}: ${renderType(d.value.tpe)}"
          )(
            i =>
              s"${d.value.nme}: ${renderType(d.value.tpe)} = ${renderValue(i)}"
          )
        case d@Statement.ValueDef.Val(_, _)  =>
          d.value.impl.fold(
            s"val ${d.value.nme}: ${renderType(d.value.tpe)}"
          )(
            i =>
              s"val ${d.value.nme}: ${renderType(d.value.tpe)} = ${renderValue(i)}"
          )