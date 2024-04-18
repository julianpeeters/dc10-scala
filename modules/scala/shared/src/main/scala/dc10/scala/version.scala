package dc10.scala

import dc10.compile.Renderer
import dc10.scala.Statement.TypeDef.{Alias, Match}
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}
import dc10.scala.Symbol.Term.ValueLevel.{App, Lam}

object version:

  given `3.4.0`: Renderer["scala-3.4.0", Error, List[Statement]] =
    new Renderer["scala-3.4.0", Error, List[Statement]]:

      override def render(input: List[Statement]): String =
        
        input.map(stmt => stmt match
          case d@Statement.CaseClassDef(_, _)        => indent(d.indent) ++ renderCaseClass(d.caseclass)
          case d@Statement.ExtensionDef(_, _)        => indent(d.indent) ++ renderExtension(d.extension)
          case d@Statement.ImportDefs(_, _)          => indent(d.indent) ++ renderImports(d.terms)
          case d@Statement.LibraryDependency(_,_,_)  => indent(d.indent) ++ renderLibraryDependency(d)
          case d@Statement.ObjectDef(_, _)           => indent(d.indent) ++ renderObject(d.obj)
          case d@Statement.PackageDef(_, _)          => indent(d.indent) ++ renderPackage(d.pkg)
          case d@Statement.TraitDef(_, _)            => indent(d.indent) ++ renderTraitDef(d)
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

      private def renderCaseClass[T](cls: CaseClass[T]) =
        s"case class ${cls.nme}(${
          if cls.fields.length <= 1
          then render(cls.fields)
          else "\n" ++ render(cls.fields) ++ "\n"
        })"

      private def renderExtension(ext: Extension) =
        s"extension (${render(List(ext.field)).mkString})\n  ${render(ext.body)}\n"

      private def renderFieldDef[T](d: Statement.ValueDef.Fld[T], input: List[Statement]): String =
        if input.length <= 1
        then renderValueDef(d)
        else indent(d.indent + 1) ++ renderValueDef(d) ++ ","

      private def renderImports(terms: List[Term]): String =
        terms.map(t =>
          t match
            case trm@Term.TypeLevel.App.App1(tfun, targ) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.App.App1T(tfun, farg, aarg) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.App.App2(tfun, ta, tb) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.App.Infix(tfun, ta, tb) => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Lam.Function1Type() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Lam.Function2Type() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.BooleanType() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.__() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.IntType() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.NothingType() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.StringType() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.UnitType() => s"import ${renderType(trm)}"
            case trm@Term.TypeLevel.Var.UserDefinedType(nme, tparams, impl) => s"import ${renderType(trm)}"
            case trm@Term.ValueLevel.App.App1(fun, arg, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.App2(fun, arg1, arg2, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.AppPure(fun, arg, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.AppVargs(fun, tpe, vargs*) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.Dot0(fun, arg1, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.Dot1(fun, arg1, arg2, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.App.Dotless(fun, arg1, arg2, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Blc.ForComp(gens, ret, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Lam.Lam1(a, b, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Lam.Lam2(a1, a2, c, tpe) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Var.BooleanLiteral(tpe, b) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Var.IntLiteral(tpe, i) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Var.StringLiteral(tpe, s) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Var.UnitLiteral(tpe, u) => s"import ${renderValue(trm)}"
            case trm@Term.ValueLevel.Var.UserDefinedValue(nme, tpe, tparams, impl) => s"import ${renderValue(trm)}"
          ).mkString("\n") ++ "\n"

      private def renderLibraryDependency(libDep: Statement.LibraryDependency): String =
        s""""${libDep.org}" %%% "${libDep.nme}" % "${libDep.ver}""""

      private def renderObject[T](obj: Object[T]): String =
        obj.par.fold(s"object ${obj.nme}:\n\n${render(obj.body)}")(p =>
          s"object ${obj.nme} extends ${renderType(p)}:\n\n${render(obj.body)}"
        )

      private def renderPackage(pkg: Package): String =
        pkg match
          case Package.Basic(nme, pkgdef) => s"package ${nme}\n\n${renderPackage(pkgdef.pkg)}"
          case Package.Empty(ms) => render(ms)
        
      private def renderType[T](tpe: Term.TypeLevel[T]): String =
        tpe match
          // application
          case Term.TypeLevel.App.App1(tfun, targ) => s"${renderType(tfun)}[${renderType(targ)}]"
          case Term.TypeLevel.App.App1T(tfun, farg, aarg) => s"${renderType(tfun)}[${renderType(farg)}, ${renderType(aarg)}]"
          case Term.TypeLevel.App.App2(tfun, ta, tb) => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
          case Term.TypeLevel.App.App2T(tfun, ta1, ta2, tb) => s"${renderType(tfun)}[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(tb)}]"
          case Term.TypeLevel.App.App3(tfun, ta1, ta2, tb) => s"${renderType(ta1)} ${renderType(tfun)} ${renderType(tb)}"
          case Term.TypeLevel.App.Infix(tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          // primitive
          case Term.TypeLevel.Var.__() => "_"
          case Term.TypeLevel.Var.BooleanType() => "Boolean"
          case Term.TypeLevel.Var.IntType() => "Int"
          case Term.TypeLevel.Var.NothingType() => "Nothing"
          case Term.TypeLevel.Var.StringType() => "String"
          case Term.TypeLevel.Var.UnitType() => "Unit"
          // complex
          case Term.TypeLevel.Lam.Function1Type() => "=>"
          case Term.TypeLevel.Lam.Function2Type() => "=>"
          case Term.TypeLevel.Var.UserDefinedType(s, p, i) => if p.isEmpty then s else s"${s}[${p.map(renderType).mkString(", ")}]"

      private def renderTraitDef(traitDef: Statement.TraitDef): String =
        val params = traitDef.`trait`.tParams
        s"""trait ${traitDef.`trait`.nme}${if params.isEmpty then "" else s"[${params.map(p => renderType(p)).mkString(", ")}]"}:
          |${traitDef.`trait`.body.map(s => indent(s.indent + 1) + render(List(s))).mkString("\n")}""".stripMargin

      private def renderTypeDef(typeDef: Statement.TypeDef): String =
        typeDef match
          case d@Statement.TypeDef.Alias(i, s) => d.tpe.impl.fold(
            s"type ${renderType(d.tpe)}"
          )(i =>
            s"type ${renderType(d.tpe)} = ${renderType(i)}"
          )
          // case d@Statement.TypeDef.AliasF(i, s) => //d.tpe //  .impl.fold(
          //   s"type ${renderType(d.tpe.tfun)}[${renderType(d.tpe.targ)}]"
          // // )(i =>
          // //   s"type ${d.tpe.nme} = ${renderType(i)}"
          // // )
          // case d@Statement.TypeDef.AliasFA(i, s) => //d.tpe //  .impl.fold(
          //   s"type ${renderType(d.tpe.tfun)}[${renderType(d.tpe.farg)}, ${renderType(d.tpe.aarg)}]"
          // // )(i =>
          // //   s"type ${d.tpe.nme} = ${renderType(i)}"
          // // )
          case d@Match(i, s) =>
            s"""|type ${renderType(d.tpe)} = ${renderType(d.tpe.targ)} match
                |${d.rhs.map(app => indent(d.indent + 1) ++ "case " ++ renderType(app)).toList.mkString("\n")}""".stripMargin
            
      private def renderValue[T](value: Term.ValueLevel[T]): String =
        value match 
          // application
          case Term.ValueLevel.App.App1(f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
          case Term.ValueLevel.App.App2(f, a, b, t) => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)})"
          case Term.ValueLevel.App.AppPure(f, a, t) => s"${renderValue(f)}(${renderValue(a)})"
          case Term.ValueLevel.App.AppVargs(f, t, as*) => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case Term.ValueLevel.App.Dot0(f, a, t) => s"${renderValue(a)}.${renderValue(f)}"
          case Term.ValueLevel.App.Dot1(f, a, b, t) => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case Term.ValueLevel.App.Dotless(f, a, b, t) => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          // block
          case Term.ValueLevel.Blc.ForComp(l, v, t) => s"\n  for\n${render(l.map(s => s.addIndent))}\n  yield ${renderValue(v)}"
          // function
          case Term.ValueLevel.Lam.Lam1(a, b, t) => s"${renderValue(a)} => ${renderValue(b)}"
          case Term.ValueLevel.Lam.Lam2(a1, a2, b, t) => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(b)}"
          // primitive
          case Term.ValueLevel.Var.BooleanLiteral(tpe, b) => s"$b"
          case Term.ValueLevel.Var.IntLiteral(tpe, i) => s"$i"
          case Term.ValueLevel.Var.StringLiteral(tpe, s) => s"\"$s\""
          case Term.ValueLevel.Var.UnitLiteral(tpe, u) => s"$u"
          // complex
          case Term.ValueLevel.Var.UserDefinedValue(s, t, tparams, i) =>
            if tparams.isEmpty then s else s"$s[${tparams.map(renderType).mkString(", ")}]"

      private def renderValueDef(valueDef: Statement.ValueDef): String =
        valueDef match
          case d@Statement.ValueDef.Def(_, _) =>
            d.ret.fold(
              s"def ${renderValue(d.value)}${d.arg.fold("")(a => s"(${renderValue(a)}: ${renderType(a.tpe)})")}: ${renderType(d.tpe)}"
            )(
              i => s"def ${renderValue(d.value)}${d.arg.fold("")(a => s"(${renderValue(a)}: ${renderType(a.tpe)})")}: ${renderType(d.tpe)} = ${renderValue(i)}"
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