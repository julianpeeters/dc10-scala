package dc10.scala.version

import dc10.compile.Renderer
import dc10.scala.Statement
import dc10.scala.Statement.{CaseClassDef, PackageDef, TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{Object, Package, Term}
import dc10.scala.Symbol.Term.ValueLevel.{AppCtor1, AppVargs, Lam1}
import dc10.scala.ctx.CompilerError

given `3.3.1`: Renderer["scala-3.3.1", CompilerError, List[Statement]] =
  new Renderer["scala-3.3.1", CompilerError, List[Statement]]:

    override def render(input: List[Statement]): String = input.map(stmt => stmt match
      case d@CaseClassDef(_, _) =>
        indent(d.indent) ++ s"case class ${d.caseclass.nme}(${render(d.caseclass.fields).mkString})"
      case d@Statement.ObjectDef(_, _) =>
        indent(d.indent) ++ renderObject(d.obj)
      case d@PackageDef(_, _) =>
        indent(d.indent) ++ renderPackage(d.pkg)
      case d@ValueDef.Def(_, _) =>
        indent(d.indent) ++ renderValueDef(d)
      case d@ValueDef.Val(_, _) =>
        indent(d.indent) ++ renderValueDef(d)
      case e@TypeExpr(t) => 
        indent(e.indent) ++ renderType(t.tail.value)
      case e@ValueExpr(v) => 
        indent(e.indent) ++ renderValue(v.tail.value)
    ).mkString("\n")

    override def renderErrors(errors: List[CompilerError]): String =
      errors.map(_.toString()).mkString("\n")

    override def version: "scala-3.3.1" =
      "scala-3.3.1"

    private def indent(i: Int): String =
      "  ".repeat(i)

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
        // primitive
        case Term.TypeLevel.Var.BooleanType(_) => "Boolean"
        case Term.TypeLevel.Var.IntType(_) => "Int"
        case Term.TypeLevel.Var.StringType(_) => "String"
        // complex
        case Term.TypeLevel.Var.Function1Type(_) => "=>"
        case Term.TypeLevel.Var.ListType(_) => "List"
        case Term.TypeLevel.Var.OptionType(_) => "Option"
        case Term.TypeLevel.Var.UserDefinedType(q, s, i) => s

    private def renderValue[T, X](value: Term.ValueLevel[T, X]): String =
      value match 
        // application
        case Term.ValueLevel.App1(q, f, a) => s"${renderValue(f.tail.value)}(${renderValue(a.tail.value)})"
        case Term.ValueLevel.AppCtor1(q, t, a) => s"${renderType(t.tail.value)}(${renderValue(a.tail.value)})"
        case Term.ValueLevel.AppVargs(q, f, as*) => s"${renderValue(f.tail.value)}(${as.map(a => renderValue(a.tail.value)).mkString(", ")})"
        // function
        case Term.ValueLevel.Lam1(q, a, b) => s"${renderValue(a.tail.value)} => ${renderValue(b.tail.value)}"
        // primitive
        case Term.ValueLevel.Var.BooleanLiteral(q, b) => s"$b"
        case Term.ValueLevel.Var.IntLiteral(q, i) => s"$i"
        case Term.ValueLevel.Var.StringLiteral(q, s) => s"\"${s}\""
        // complex
        case Term.ValueLevel.Var.ListCtor(q) => s"List"
        case Term.ValueLevel.Var.OptionCtor(q) => s"Option"
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
        case d@ValueDef.Val(_, _)  =>
          d.value.impl.fold(
            s"val ${d.value.nme}: ${renderType(d.value.tpe.tail.value)}"
          )(
            i =>
              s"val ${d.value.nme}: ${renderType(d.value.tpe.tail.value)} = ${renderValue(i.tail.value)}"
          )