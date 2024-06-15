package dc10.scala

import dc10.compile.Renderer
import dc10.scala.Statement.TraitDef.{`trait`, `trait[_]`, `trait[_[_]]`, `trait[_[_], _]`}
import dc10.scala.Symbol.{CaseClass, Extension, Object, Package, Term}

object version:

  given `3.3.3`: Renderer["scala-3.3.3", Error, List[Statement]] =
    new Renderer["scala-3.3.3", Error, List[Statement]]:

      override def render(input: List[Statement]): String =
        input.map(stmt => stmt match
          case d@Statement.`case class`(_, _, _)                              => renderIndent(d.indent) ++ renderCaseClass(d.caseclass)
          case d@Statement.extension(_, _, _)                                 => renderIndent(d.indent) ++ renderExtension(d.extension)
          case d@Statement.`object`(_, _, _)                                  => renderIndent(d.indent) ++ renderObject(d.obj)
          case d@Statement.`package`(_, _, _)                                 => renderIndent(d.indent) ++ renderPackage(d.pkg)
          case d@Statement.TraitDef.`trait`(_, _, _)                          => renderIndent(d.indent) ++ renderTraitDef(d)
          case d@Statement.TraitDef.`trait[_]`(_, _, _, _)                    => renderIndent(d.indent) ++ renderTraitDef(d)
          case d@Statement.TraitDef.`trait[_[_]]`(_, _, _, _)                 => renderIndent(d.indent) ++ renderTraitDef(d)
          case d@Statement.TraitDef.`trait[_[_], _]`(_, _, _, _, _)           => renderIndent(d.indent) ++ renderTraitDef(d)
          case d@Statement.TypeDef.`Alias`(_, _, _)                           => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.TypeDef.`Alias[_]=>>`(_, _, _)                     => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.TypeDef.`Alias[_]`(_, _, _, _)                     => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.TypeDef.`Alias[_[_]]`(_, _, _, _)                  => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.TypeDef.`Alias[_[_], _]`(_, _, _, _, _)            => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.TypeDef.`Match`(_, _, _, _)                        => renderIndent(d.indent) ++ renderTypeDef(d)
          case d@Statement.ValueDef.`def`(_, _, _, _, _, _)                   => renderIndent(d.indent) ++ renderValueDef(d)
          case d@Statement.ValueDef.`def[_]`(_, _, _, _, _)                   => renderIndent(d.indent) ++ renderValueDef(d)
          case d@Statement.ValueDef.`def[_[_]]`(_, _, _, _, _)                => renderIndent(d.indent) ++ renderValueDef(d)
          case d@Statement.ValueDef.Fld(_, _, _)                              => renderIndent(d.indent) ++ renderFieldDef(d, input)
          case d@Statement.ValueDef.Gen(_, _, _, _)                           => renderIndent(d.indent) ++ renderValueDef(d)
          case d@Statement.ValueDef.`val`(_, _, _, _)                         => renderIndent(d.indent) ++ renderValueDef(d)
          case e@Statement.TypeExpr.`Type`(t)                                 => renderType(t)
          case e@Statement.TypeExpr.`Type[_]`(t)                              => renderType(t)
          case e@Statement.TypeExpr.`Type[_[_]]`(t)                           => renderType(t)
          case e@Statement.TypeExpr.`Type[_, _]`(t)                           => renderType(t)
          case e@Statement.TypeExpr.`Type[_[_], _]`(t)                        => renderType(t)
          case e@Statement.ValueExpr.`Value`(v)                               => renderValue(v)
        ).mkString("\n")

      override def renderErrors(errors: List[Error]): String =
        errors.map(_.toString()).mkString("\n")

      override def version: "scala-3.3.3" =
        "scala-3.3.3"

      private def renderIndent(i: Int): String =
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
        else renderIndent(d.indent + 1) ++ renderValueDef(d) ++ ","

      private def renderObject[T](obj: Object[T]): String =
        obj.parent.fold(s"object ${obj.nme}:\n\n${render(obj.body)}")(p =>
          s"object ${obj.nme} extends ${renderType(p)}:\n\n${render(obj.body)}"
        )

      private def renderPackage(pkg: Package): String =
        pkg match
          case Package.Basic(nme, pkgdef) => s"package ${nme}\n\n${renderPackage(pkgdef.pkg)}"
          case Package.Empty(ms) => render(ms)
        
      private def renderType[T](tpe: Term.TypeLevel): String =
        tpe match
          case Term.TypeLevel.App.`App[_]`(tfun, targ)               => s"${renderType(tfun)}[${renderType(targ)}]"
          case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg)   => s"${renderType(tfun)}[${renderType(farg)}, ${renderType(aarg)}]"
          case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb)          => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
          case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => s"${renderType(ta1)} ${renderType(tfun)} ${renderType(tb)}"
          case Term.TypeLevel.App.Infix(tfun, ta, tb)                => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc)           => s"(${renderType(ta)}, ${renderType(tb)}) ${renderType(tfun)} ${renderType(tc)}"
          case Term.TypeLevel.Lam.Lam(ta, tb)                        => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case Term.TypeLevel.Var.`UserDefinedType`(s, i)            => s
          case Term.TypeLevel.Var.`UserDefinedType[_]`(s, i)         => s
          case Term.TypeLevel.Var.`UserDefinedType[_[_]]`(s, i)      => s
          case Term.TypeLevel.Var.`UserDefinedType[_, _]`(s, i)      => s
          case Term.TypeLevel.Var.`UserDefinedType[_, _, _]`(s, i)   => s
          case Term.TypeLevel.Var.`UserDefinedType[_[_], _]`(s, i)   => s

      private def renderTraitDef[T](traitDef: Statement.TraitDef): String =
        traitDef match
          case d@`trait`(indent, sp, t) =>
            s"""trait ${d.`trait`.nme}:
                |${d.`trait`.body.map(s => "  " + render(List(s))).mkString("\n")}""".stripMargin
          case d@`trait[_]`(indent, sp, tparam, t) =>
             s"""trait ${d.`trait`.nme}[${renderType(d.tparam)}]:
                |${d.`trait`.body.map(s => "  " + render(List(s))).mkString("\n")}""".stripMargin
          case d@`trait[_[_]]`(indent, sp, tparam, t) =>
             s"""trait ${d.`trait`.nme}[${renderType(d.tparam)}[_]]:
                |${d.`trait`.body.map(s => "  " + render(List(s))).mkString("\n")}""".stripMargin
          case d@`trait[_[_], _]`(indent, sp, tparamF, tparamA, t) =>
             s"""trait ${d.`trait`.nme}[${renderType(d.tparamF)}[_], ${renderType(d.tparamA)}]:
                |${d.`trait`.body.map(s => "  " + render(List(s))).mkString("\n")}""".stripMargin
        
      private def renderTypeDef(typeDef: Statement.TypeDef): String =
        typeDef match
          case d@Statement.TypeDef.`Alias`(i, s, t)                => t.impl.fold(s"type ${renderType(t)}")(i => s"type ${renderType(d.tpe)} = ${renderType(i)}")
          case d@Statement.TypeDef.`Alias[_]=>>`(i, s, t)          => t.impl.fold(s"type ${renderType(t)}")(i => s"type ${renderType(t)} = ${renderType(i)}")
          case d@Statement.TypeDef.`Alias[_]`(i, s, a, t)          => t.impl.fold(s"type ${renderType(t)}[${renderType(a)}]")(i => s"type ${renderType(t)}[${renderType(a)}] = ${renderType(i)}")
          case d@Statement.TypeDef.`Alias[_[_]]`(i, s, a, t)       => t.impl.fold(s"type ${renderType(t)}[${renderType(a)}[_]]")(i => s"type ${renderType(t)}[${renderType(a)}[_]] = ${renderType(i)}")
          case d@Statement.TypeDef.`Alias[_[_], _]`(_, _, f, a, t) => t.impl.fold(s"type ${renderType(t)}[${renderType(f)}[_], ${renderType(a)}]")(i => s"type ${renderType(t)}[${renderType(f)}[_], ${renderType(a)}] = ${renderType(i)}")
          case d@Statement.TypeDef.Match(i, s, t, l) =>
            s"""|type ${renderType(d.tpe)} = ${renderType(d.tpe.aarg)} match
                |${d.rhs.map(app => renderIndent(d.indent + 1) ++ "case " ++ renderType(app)).toList.mkString("\n")}""".stripMargin

      private def renderValue(value: Term.ValueLevel): String =
        value match 
          case Term.ValueLevel.App.App1(f, a, t)                  => s"${renderValue(f)}(${renderValue(a)})"
          case Term.ValueLevel.App.App2(f, a, b, t)               => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)})"
          case Term.ValueLevel.App.AppPure(f, a, t)               => s"${renderValue(f)}(${renderValue(a)})"
          case Term.ValueLevel.App.AppVargs(f, t, as*)            => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case Term.ValueLevel.App.Dot0(f, a, t)                  => s"${renderValue(a)}.${renderValue(f)}"
          case Term.ValueLevel.App.Dot1(f, a, b, t)               => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case Term.ValueLevel.App.Dotless(f, a, b, t)            => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          case Term.ValueLevel.App.ForComp(l, v, t)               => s"\n  for\n${render(l.map(s => s.addIndent))}\n  yield ${renderValue(v)}"
          case Term.ValueLevel.Lam.Lam1(a, b, t)                  => s"${renderValue(a)} => ${renderValue(b)}"
          case Term.ValueLevel.Lam.Lam2(a1, a2, b, t)             => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(b)}"
          case Term.ValueLevel.Var.BooleanLiteral(tpe, b)         => s"$b"
          case Term.ValueLevel.Var.IntLiteral(tpe, i)             => s"$i"
          case Term.ValueLevel.Var.StringLiteral(tpe, s)          => s"\"$s\""
          case Term.ValueLevel.Var.UnitLiteral(tpe, u)            => s"$u"
          case Term.ValueLevel.Var.`UserDefinedValue`(s, t, i)    => s
          case Term.ValueLevel.Var.`UserDefinedValue[_]`(s, _, _) => s

      private def renderValueDef(valueDef: Statement.ValueDef): String =
        valueDef match
          case d@Statement.ValueDef.`def`(i, sp, ret, arg, tpe, value) =>
            d.impl.fold(
              s"def ${renderValue(d.value)}${d.arg.fold("")(a => s"(${renderValue(a)}: ${renderType(a.tpe)})")}: ${renderType(d.tpe)}"
            )(
              i => s"def ${renderValue(d.value)}${d.arg.fold("")(a => s"(${renderValue(a)}: ${renderType(a.tpe)})")}: ${renderType(d.tpe)} = ${renderValue(i)}"
            )
          case d@Statement.ValueDef.`def[_]`(i, sp, tparam, ret, value) =>
            d.impl.fold(
              s"def ${renderValue(d.value)}[${renderType(tparam)}]: ${renderType(d.value.tpe)}"
            )(
              i => s"def ${renderValue(d.value)}[${renderType(tparam)}]: ${renderType(d.value.tpe)} = ${renderValue(i)}"
            )
          case d@Statement.ValueDef.`def[_[_]]`(i, sp, tparam, ret, value) =>
            d.impl.fold(
              s"def ${renderValue(d.value)}[${renderType(tparam)}[_]]: ${renderType(d.value.tpe)}"
            )(
              i => s"def ${renderValue(d.value)}[${renderType(tparam)}[_]]: ${renderType(d.value.tpe)} = ${renderValue(i)}"
            )
          case d@Statement.ValueDef.Fld(_, _, _)  =>
            d.value.impl.fold(
              s"${d.value.nme}: ${renderType(d.value.tpe)}"
            )(
              i => s"${d.value.nme}: ${renderType(d.value.tpe)} = ${renderValue(i)}"
            )
          case d@Statement.ValueDef.Gen(_, _, _, _)  =>
            s"  ${d.value.nme} <- ${renderValue(d.impl)}"
          case d@Statement.ValueDef.`val`(_, _, _, _)  =>
            d.value.impl.fold(
              s"val ${d.value.nme}: ${renderType(d.value.tpe)}"
            )(
              i => s"val ${d.value.nme}: ${renderType(d.value.tpe)} = ${renderValue(i)}"
            )