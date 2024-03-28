package dc10.scala.version

import dc10.compile.Renderer
import dc10.scala.Statement
import dc10.scala.Statement.TypeDef.{Alias, Match}
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}
import dc10.scala.Symbol.Term.ValueLevel.{App, Lam}
import dc10.scala.Error

given `3.4.0`: Renderer["scala-3.4.0", Error, List[Statement]] =
  new Renderer["scala-3.4.0", Error, List[Statement]]:

    override def render(input: List[Statement]): String =
      
      input.map(stmt => stmt match
        case d@Statement.CaseClassDef(_, _)        => indent(d.indent) ++ renderCaseClass(d.caseclass)
        case d@Statement.ExtensionDef(_, _)        => indent(d.indent) ++ renderExtension(d.extension)
        case d@Statement.ImportDefs(_, _)          => indent(d.indent) ++ renderImports(d.terms)
        case d@Statement.ObjectDef(_, _)           => indent(d.indent) ++ renderObject(d.obj)
        case d@Statement.PackageDef(_, _)          => indent(d.indent) ++ renderPackage(d.pkg)
        case d@Statement.TypeDef.Alias(_, _)       => indent(d.indent) ++ renderTypeDef(d)
        case d@Statement.TypeDef.Match(_, _)       => indent(d.indent) ++ renderTypeDef(d)
        case d@Statement.ValueDef.Def(_, _)        => indent(d.indent) ++ renderValueDef(d)
        case d@Statement.ValueDef.Fld(_, _)        => indent(d.indent) ++ renderFieldDef(d, input)
        case d@Statement.ValueDef.Gen(_, _)        => indent(d.indent) ++ renderValueDef(d)
        case d@Statement.ValueDef.Val(_, _)        => indent(d.indent) ++ renderValueDef(d)
        case e@Statement.TypeExpr(t)               => indent(e.indent) ++ renderType(t)
        case e@Statement.ValueExpr(v)              => indent(e.indent) ++ renderValue(v)
      ).mkString("\n")

    override def renderErrors(errors: List[Error]): String =
      errors.map(_.toString()).mkString("\n")

    override def version: "scala-3.4.0" =
      "scala-3.4.0"

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

    private def renderImports(terms: List[Term]): String =
      terms.map(t =>
        t match
          case trm@Term.TypeLevel.App.App1(qnt, tfun, targ, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.App1T(qnt, tfun, farg, aarg, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.App2T(qnt, tfun, ta1, ta2, tb, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.App.InfixPi(qnt, tfun, a, tb, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Lam.Function1Type(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Lam.Function2Type(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.BooleanType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.IntType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.StringType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.UnitType(qnt) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.ListType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.OptionType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.SomeType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.TupleType(qnt, dep) => s"import ${renderType(trm)}"
          case trm@Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => s"import ${renderType(trm)}"

          case trm@Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Blc.ForComp(qnt, gens, ret, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Lam.Lam2(qnt, a1, a2, c, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.UnitLiteral(qnt, tpe, u) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.ListCtor(qnt, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.OptionCtor(qnt, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.SomeCtor(qnt, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.TupleCtor(qnt, tpe) => s"import ${renderValue(trm)}"
          case trm@Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => s"import ${renderValue(trm)}"
        ).mkString("\n") ++ "\n"

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
        case Term.TypeLevel.App.App1T(qnt, tfun, farg, aarg, z) => s"${renderType(tfun)}[${renderType(farg)}, ${renderType(aarg)}]"
        case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, z) => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
        case Term.TypeLevel.App.App2T(qnt, tfun, ta1, ta2, tb, z) => s"${renderType(tfun)}[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(tb)}]"
        case Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, z) => s"${renderType(ta1)} ${renderType(tfun)} ${renderType(tb)}"
        case Term.TypeLevel.App.Infix(qnt, tfun, ta, tb, z) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
        case Term.TypeLevel.App.InfixPi(qnt, tfun, a, tb, z) => s"${renderValue(a)} ${renderType(tfun)} ${renderType(tb)}"
        // primitive
        case Term.TypeLevel.Var.BooleanType(_, z) => "Boolean"
        case Term.TypeLevel.Var.IntType(_, z) => "Int"
        case Term.TypeLevel.Var.StringType(_, z) => "String"
        case Term.TypeLevel.Var.UnitType(_) => "Unit"
        // complex
        case Term.TypeLevel.Lam.Function1Type(_, z) => "=>"
        case Term.TypeLevel.Lam.Function2Type(_, z) => "=>"
        case Term.TypeLevel.Var.ListType(_, z) => "List"
        case Term.TypeLevel.Var.OptionType(_, z) => "Option"
        case Term.TypeLevel.Var.SomeType(_, z) => "Some"
        case Term.TypeLevel.Var.TupleType(_, z) => "Tuple2"
        case Term.TypeLevel.Var.UserDefinedType(q, s, i, z) => s

    private def renderTypeDef(typeDef: Statement.TypeDef): String =
      typeDef match
        case d@Alias(i, s) => d.tpe.impl.fold(
          s"type ${d.tpe.nme}"
        )(i =>
          s"type ${d.tpe.nme} = ${renderType(i)}"
        )
        case d@Match(i, s) =>
          s"""|type ${renderType(d.tpe)} = ${renderType(d.tpe.targ)} match
              |${d.rhs.map(app => indent(d.indent + 1) ++ "case " ++ renderType(app)).toList.mkString("\n")}""".stripMargin
          
    private def renderValue[Z, T, X](value: Term.ValueLevel[T, Z]): String =
      value match 
        // application
        case Term.ValueLevel.App.App1(q, f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppCtor1(q, t, a) => s"${renderType(t)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppCtor2(q, n, t, a, b) => s"$n(${renderValue(a)}, ${renderValue(b)})"
        case Term.ValueLevel.App.AppPure(q, f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
        case Term.ValueLevel.App.AppVargs(q, f, t, as*) => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
        case Term.ValueLevel.App.Dot1(q, f, a, b, t) => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
        case Term.ValueLevel.App.Dotless(q, f, a, b, t) => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
        // block
        case Term.ValueLevel.Blc.ForComp(q, l, v, t) => s"\n  for\n${render(l.map(s => s.addIndent))}\n  yield ${renderValue(v)}"
        // function
        case Term.ValueLevel.Lam.Lam1(q, a, b, t) => s"${renderValue(a)} => ${renderValue(b)}"
        case Term.ValueLevel.Lam.Lam2(q, a1, a2, b, t) => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(b)}"
        // primitive
        case Term.ValueLevel.Var.BooleanLiteral(q, tpe, b) => s"$b"
        case Term.ValueLevel.Var.IntLiteral(q, tpe, i) => s"$i"
        case Term.ValueLevel.Var.StringLiteral(q, tpe, s) => s"\"${s}\""
        case Term.ValueLevel.Var.UnitLiteral(q, tpe, u) => s"$u"
        // complex
        case Term.ValueLevel.Var.ListCtor(q, tpe) => s"List"
        case Term.ValueLevel.Var.OptionCtor(q, tpe) => s"Option"
        case Term.ValueLevel.Var.SomeCtor(q, tpe) => s"Some"
        case Term.ValueLevel.Var.TupleCtor(q, tpe) => s""
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
        case d@Statement.ValueDef.Gen(_, _)  =>
          s"  ${d.value.nme} <- ${renderValue(d.impl)}"
        case d@Statement.ValueDef.Val(_, _)  =>
          d.value.impl.fold(
            s"val ${d.value.nme}: ${renderType(d.value.tpe)}"
          )(
            i =>
              s"val ${d.value.nme}: ${renderType(d.value.tpe)} = ${renderValue(i)}"
          )