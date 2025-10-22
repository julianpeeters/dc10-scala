package dc10.scala

import dc10.Renderer
import dc10.scala.internal.indent.getIndent

object version:

  given `3.3.7`: Renderer[Statement, Error, "scala-3.3.7"] =
    new Renderer[Statement, Error, "scala-3.3.7"]:

      override def render(input: List[Statement]): String =
        input.map(stmt => stmt match
          case d@Statement.`case`(t)                               => renderCaseExpr(d)
          case d@Statement.`case class`(_, _, _)                   => renderCaseClassDef(d)
          case d@Statement.`def`.`0`(_)                            => renderValDef(d)
          case d@Statement.`def`.`0`.`[_]`(_, _, _)                => renderValDef(d)
          case d@Statement.`def`.`0`.`[_[_]]`(_, _, _)             => renderValDef(d)
          case d@Statement.`def`.`0`.`[_[_], _]`(_, _, _, _)       => renderValDef(d)
          case d@Statement.`def`.`1`(_, _, _, _)                   => renderValDef(d)
          case d@Statement.`def`.`1`.`[_]`(_, _, _, _, _)          => renderValDef(d)
          case d@Statement.`def`.`1`.`[_[_], _]`(_, _, _, _, _, _) => renderValDef(d)
          case d@Statement.`extension`(_, _)                       => renderExtensionDef(d)
          case d@Statement.`field`(_)                              => renderValDef(d)
          case d@Statement.`generator`(_)                          => renderGenerator(d)
          case d@Statement.`object`(_, _, _)                       => renderObjectDef(d)
          case d@Statement.`package`(_, _)                         => renderPackageDef(d)
          case d@Statement.`trait`(_, _, _)                        => renderTraitDef(d)
          case d@Statement.`trait`.`[_]`(_, _, _, _)               => renderTraitDef(d)
          case d@Statement.`trait`.`[_[_]]`(_, _, _, _)            => renderTraitDef(d)
          case d@Statement.`trait`.`[_[_], _]`(_, _, _, _, _)      => renderTraitDef(d)
          case d@Statement.`type`(_)                               => renderTypeDef(d)
          case d@Statement.`type`.`[_]=>>`(_)                      => renderTypeDef(d)
          case d@Statement.`type`.`[_]`(_, _)                      => renderTypeDef(d)
          case d@Statement.`type`.`[_[_]]`(_, _)                   => renderTypeDef(d)
          case d@Statement.`type`.`[_[_], _]`(_, _, _)             => renderTypeDef(d)
          case d@Statement.`val`(_)                                => renderValDef(d)
        ).mkString("\n")

      override def renderErrors(errors: List[Error]): String =
        errors.map(_.toString()).toList.mkString("\n")

      override def version: "scala-3.3.7" =
        "scala-3.3.7"

      private def renderIndent(i: Int): String =
        "  ".repeat(i)

      private def renderCaseExpr[T](c: Statement.`case`[T]): String =
        renderIndent(c.lambda.getIndent) ++ s"case ${renderPattern(c.lambda)}"

      private def renderCaseClassDef[T](cls: Statement.`case class`[T]): String =
        renderIndent(cls.tpe.in) ++ s"case class ${cls.tpe.nme}(${
          if cls.fields.length <= 1
          then render(cls.fields)
          else "\n" ++ cls.fields.map(f => renderIndent(f.getIndent) ++ render(List(f))).mkString(",\n") ++ "\n"
        })"

      private def renderExtensionDef[T](ext: Statement.`extension`[T]): String =
        renderIndent(ext.field.getIndent) ++ s"extension (${ext.field.nme}: ${renderTypeExpr(ext.field.tpe)})\n${render(ext.body)}\n"

      private def renderObjectDef[T](obj: Statement.`object`[T]): String =
        renderIndent(obj.value.in) ++ (
          (obj.parent, obj.body) match
            case (None, Nil)    => s"object ${obj.value.nme}"
            case (Some(p), Nil) => s"object ${obj.value.nme} extends ${renderTypeExpr(p)}"
            case (None, b)      => s"object ${obj.value.nme}:\n\n${render(obj.body)}"
            case (Some(p), b)   => s"object ${obj.value.nme} extends ${renderTypeExpr(p)}:\n\n${render(obj.body)}"
        )

      private def renderPackageDef(pkg: Statement.`package`): String =
        pkg.nme.fold(render(pkg.contents))(n => s"package ${n}\n\n${render(pkg.contents)}")



//// can renderPattern be drastically minimized?

      private def renderPattern(value: Value): String =
        value match 
          case `Value.App.1: x`(i, f, a, t)                      => s"${renderPattern(f)}(${renderValue(a)})"
          case `Value.App.2: x`(i, f, a, b, t)                   => s"${renderPattern(f)}(${renderValue(a)}, ${renderValue(b)})"
          case `Value.App.3: x`(i, f, a, b, c, t)                => s"${renderPattern(f)}(${renderValue(a)}, ${renderValue(b)}, ${renderValue(c)})"
          case `Value.App.Vargs: x`(i, f, t, as)                => s"${renderPattern(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case `Value.AppDot.0: x`(i, f, a, t)                   => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.1: x`(i, f, a, b, t)                => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDotless: x`(i, f, a, b, t)              => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          case `Value.AppForComp: x_x x`(i, l, v, t)                => s"\n${renderIndent(i)}for\n${render(l)}\n${renderIndent(i)}yield ${renderValue(v)}"
          case `Value.App.Match`(i, a, t, l)                     => s"${renderValue(a)} match\n${render(l)}"
          case `Value.App.0: x_x`(i, f, a, t)                   => s"${renderValue(f)}"
          case `Value.App.0: lx_xl_x_x`(i, f, g, a, t)        => s"${renderValue(f)}"
          case `Value.App.0: lx_xl_x_x`(i, f, g, a, b, t)     => s"${renderValue(f)}"
          case `Value.Lam.1: x_x_x x x`(i, a, b, t)                      => s"${renderPattern(a)} => ${renderValue(b)}"
          case `Value.Lam.2: x_x_x_x x * x`(i, a1, a2, r, t)                 => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(r)}"
          case `Value.Lam.3: x_x_x_x_x x * x x`(i, a1, a2, a3, r, t)             => s"(${renderValue(a1)}, ${renderValue(a2)}, ${renderValue(a3)}) => ${renderValue(r)}"
          case `Value.Lam.1: x_x`(i, fa, t)                     => s"[] =>> ${renderTypeExpr(fa.tpe)}"
          case `Value.Lit.Boolean: x`(i, tpe, b)                 => s"$b"
          case `Value.Lit.Int: x`(i, tpe, int)                   => s"$int"
          case `Value.Lit.String: x`(i, tpe, s)                  => s"\"$s\""
          case `Value.Lit.Unit: x`(i, tpe, u)                    => s"$u"
          case `Value.Var.Unbound.Data`(i, s, t)             => s
          case `Value.Var.Bound.Data`(i, s, t, impl)         => s
          case `Value.Var0[_]`(i, s, _, _)                   => s
          case `Value.Var1[_]`(i, s, _, _)                   => s
          case `Value.Var[_[_]]`(i, s, _, _)                 => s
          case `Value.Var[_, _]`(i, s, _, _)                 => s
          case `Value.Var[_[_], _]`(i, s, _, _)              => s
          case `Value.Var[_[_], _, _]`(i, s, _, _)           => s
        
      private def renderTypeExpr[T](tpe: Type): String =
        tpe match
          case `Type.App[_]`(in, tfun, targ)                  => s"${renderTypeExpr(tfun)}[${renderTypeExpr(targ)}]"
          case `Type.App[_[_]]`(in, tfun, farg)               => s"${renderTypeExpr(tfun)}[${renderTypeExpr(farg)}]"
          case `Type.App[_[_], _]`(in, tfun, farg, aarg)      => s"${renderTypeExpr(tfun)}[${renderTypeExpr(farg)}, ${renderTypeExpr(aarg)}]"
          case `Type.App[_, _]`(in, tfun, ta, tb)             => s"${renderTypeExpr(tfun)}[${renderTypeExpr(ta)}, ${renderTypeExpr(tb)}]"
          case `Type.App[_, _, _]`(in, tfun, ta1, ta2, tb)    => s"${renderTypeExpr(tfun)}[${renderTypeExpr(ta1)}, ${renderTypeExpr(ta2)}, ${renderTypeExpr(tb)}]"
          case `Type.App[_[_], _, _]`(in, tfun, f, a, b)      => s"${renderTypeExpr(tfun)}[${renderTypeExpr(f)}, ${renderTypeExpr(a)}, ${renderTypeExpr(b)}]"
          case `Type.App[_[_[_], _]]`(in, tfun, arg)          => s"${renderTypeExpr(tfun)}[${renderTypeExpr(arg)}]"
          case `Type.App[_, _, _, _]`(in, tfun, a, b, c, d)   => s"${renderTypeExpr(tfun)}[${renderTypeExpr(a)}, ${renderTypeExpr(b)}, ${renderTypeExpr(c)}, ${renderTypeExpr(d)}]"
          case `Type.AppInfix[_, _]`(in, tfun, ta, tb)        => s"${renderTypeExpr(ta)} ${renderTypeExpr(tfun)} ${renderTypeExpr(tb)}"
          case `Type.AppInfix[_, _, _]`(in, tfun, ta, tb, tc) => s"(${renderTypeExpr(ta)}, ${renderTypeExpr(tb)}) ${renderTypeExpr(tfun)} ${renderTypeExpr(tc)}"
          case `Type.AppInfix[_, _, _, _]`(in, f, a, b, c, d) => s"(${renderTypeExpr(a)}, ${renderTypeExpr(b)}, ${renderTypeExpr(c)}) ${renderTypeExpr(f)} ${renderTypeExpr(d)}"
          case `Type.Lam: x_x`(in, ta, tb)                      => s"[${renderTypeExpr(ta)}] =>> ${renderTypeExpr(tb)}"
          case `Type.Lam: lx_xl_x_x`(in, ta1, ta2, tb)          => s"[${renderTypeExpr(ta1)}, ${renderTypeExpr(ta2)}] =>> ${renderTypeExpr(tb)}"
          case `Type.Var: x`(in, s, i)               => s
          case `Type.Var: x_x`(in, s, i, c)        => s
          // case `Type.Var2[_]`(in, s, i, _, _)     => s
          case `Type.Var: lx_xl_x`(in, s, i)         => s
          case `Type.Var: x_x_x`(in, s, i)         => s
          case `Type.Var: x_x_x_x`(in, s, i)      => s
          case `Type.Var: lx_xl_x_x`(in, s, i)      => s
          case `Type.Var: lx_xl_x_x_x`(in, s, i)   => s
          case `Type.Var: llx_xl_x_xl_x`(in, s, i)   => s
          case `Type.Var: x_x_x_x_x`(in, s, i)   => s
          case `Type.Bot: x`(in) => "Nothing"

      private def renderTraitDef[T](t: Statement.`trait`[T]): String =
        renderIndent(t.tpe.in) ++ s"""trait ${t.tpe.nme}${t.parent.fold("")(p => s" extends ${renderTypeExpr(p)}")}:
            |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      private def renderTraitDef[T[_], A](t: Statement.`trait`.`[_]`[T, A]): String =
        renderIndent(t.tpe.in) ++ s"""trait ${t.tpe.nme}[${renderTypeExpr(t.tparam)}]${t.parent.fold("")(p => s" extends ${renderTypeExpr(p)}")}:
            |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      private def renderTraitDef[T[_[_]], F[_]](t: Statement.`trait`.`[_[_]]`[T, F]): String =
        renderIndent(t.tpe.in) ++ s"""trait ${t.tpe.nme}[${renderTypeExpr(t.tparam)}[_]]${t.parent.fold("")(p => s" extends ${renderTypeExpr(p)}")}:
          |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      private def renderTraitDef[T[_[_], _], F[_], A](t: Statement.`trait`.`[_[_], _]`[T, F, A]): String =
        renderIndent(t.tpe.in) ++ s"""trait ${t.tpe.nme}[${renderTypeExpr(t.tparamf)}[_], ${renderTypeExpr(t.tparama)}]${t.parent.fold("")(p => s" extends ${renderTypeExpr(p)}")}:
            |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin
        
      private def renderTypeDef[T](t: Statement.`type`[T]): String =
        renderIndent(t.tpe.in) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}")(i => s"type ${renderTypeExpr(t.tpe)} = ${renderTypeExpr(i)}")

      private def renderTypeDef[T[_]](t: Statement.`type`.`[_]=>>`[T]): String =
        renderIndent(t.tpe.in) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}")(i => s"type ${renderTypeExpr(t.tpe)} = ${renderTypeExpr(i)}")

      private def renderTypeDef[T[_], A](t: Statement.`type`.`[_]`[T, A]): String =
        renderIndent(t.tpe.in) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}]")(i => s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}] = ${renderTypeExpr(i)}")
     
      private def renderTypeDef[T[_[_]], F[_]](t: Statement.`type`.`[_[_]]`[T, F]): String =
        renderIndent(t.tpe.in) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}[_]]")(i => s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}[_]] = ${renderTypeExpr(i)}")

      private def renderTypeDef[T[_[_], _], F[_], A](t: Statement.`type`.`[_[_], _]`[T, F, A]): String =
        renderIndent(t.tpe.in) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparamf)}[_], ${renderTypeExpr(t.tparama)}]")(i => s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparamf)}[_], ${renderTypeExpr(t.tparama)}] = ${renderTypeExpr(i)}")

      private def renderValue(value: Value): String =
        value match 
          case `Value.App.1: x`(i, f, a, t)                      => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.App.2: x`(i, f, a, b, t)                   => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)})"
          case `Value.App.3: x`(i, f, a, b, c, t)                => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)}, ${renderValue(c)})"
          case `Value.App.Vargs: x`(i, f, t, asxl                => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case `Value.AppDot.0: x`(i, f, a, t)                   => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.1: x`(i, f, a, b, t)                => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDotless: x`(i, f, a, b, t)             => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          case `Value.AppForComp: x_x x`(i, l, v, t)                => s"\n${renderIndent(i)}for\n${render(l)}\n${renderIndent(i)}yield ${renderValue(v)}"
          case `Value.App.Match`(i, a, t, l)                  => s"${renderValue(a)} match\n${render(l)}"
          case `Value.App.0: x_x`(i, f, a, t)                => s"${renderValue(f)}[${renderTypeExpr(a)}]"
          case `Value.App.0: lx_xl_x_x`(i, f, g, a, t)       => s"${renderValue(f)}[${renderTypeExpr(g)}, ${renderTypeExpr(a)}]"
          case `Value.App.0: lx_xl_x_x`(i, f, g, a, b, t) => s"${renderValue(f)}[${renderTypeExpr(g)}, ${renderTypeExpr(a)}, ${renderTypeExpr(b)}]"
          case `Value.Lam.1: x_x_x x x`(i, a, b, t)                      => s"${renderValue(a)} => ${renderValue(b)}"
          case `Value.Lam.2: x_x_x_x x * x`(i, a1, a2, r, t)                 => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(r)}"
          case `Value.Lam.3: x_x_x_x_x x * x x`(i, a1, a2, a3, r, t)             => s"(${renderValue(a1)}, ${renderValue(a2)}, ${renderValue(a3)}) => ${renderValue(r)}"
          case `Value.Lam.1: x_x`(i, fa, t)                     => s"[] =>> ${renderTypeExpr(fa.tpe)}"
          case `Value.Lit.Boolean: x`(i, tpe, b)                 => s"$b"
          case `Value.Lit.Int: x`(i, tpe, int)                   => s"$int"
          case `Value.Lit.String: x`(i, tpe, s)                  => s"\"$s\""
          case `Value.Lit.Unit: x`(i, tpe, u)                    => s"$u"
          case `Value.Var.Unbound.Data`(i, s, t)                         => s
          case `Value.Var.Bound.Data`(i, s, t, impl)                   => s
          case `Value.Var0[_]`(i, s, _, _)                   => s
          case `Value.Var1[_]`(i, s, _, _)                   => s
          case `Value.Var[_[_]]`(i, s, _, _)                 => s
          case `Value.Var[_, _]`(i, s, _, _)                 => s
          case `Value.Var[_[_], _]`(i, s, _, _)              => s
          case `Value.Var[_[_], _, _]`(i, s, _, _)           => s

      private def renderValDef[A](d: Statement.`def`.`0`[A]): String =
        d.value match
          case `Value.Var.Unbound.Data`(in, nme, tpe) =>
            renderIndent(in) ++ s"def ${renderValue(d.value)}: ${renderTypeExpr(d.value.tpe)}"
          case `Value.Var.Bound.Data`(in, nme, tpe, impl) =>
            renderIndent(in) ++ s"def ${renderValue(d.value)}: ${renderTypeExpr(d.value.tpe)} = ${renderValue(impl)}"

      private def renderValDef[T, A](d: Statement.`def`.`0`.`[_]`[T, A]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
        )

      private def renderValDef[F[_], A](d: Statement.`def`.`0`.`[_[_]]`[F, A]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
        )

      private def renderValDef[F[_], T, A](d: Statement.`def`.`0`.`[_[_], _]`[F, A, T]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
        )

      private def renderValDef[A, B](d: Statement.`def`.`1`[A, B]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)} = ${renderValue(i)}"
        )

      private def renderValDef[T, A, B](d: Statement.`def`.`1`.`[_]`[T, A, B]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)} = ${renderValue(i)}"
        )

      private def renderValDef[F[_], T, A, B](d: Statement.`def`.`1`.`[_[_], _]`[F, T, A, B]): String =
        d.impl.fold(
          renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}[${renderTypeExpr(d.tparamf)}, ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)}"
        )(
          i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}(${renderValue(d.arg)}[${renderTypeExpr(d.tparamf)}, ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.arg.tpe)}): ${renderTypeExpr(d.ret)} = ${renderValue(i)}"
        )

      private def renderValDef[T](d: Statement.field[T]): String =
        d.value match
          case `Value.Var.Unbound.Data`(in, nme, tpe) => s"${nme}: ${renderTypeExpr(tpe)}"
          case `Value.Var.Bound.Data`(in, nme, tpe, impl) => s"${nme}: ${renderTypeExpr(tpe)} = ${renderValue(impl)}"

      private def renderGenerator[F[_], A](v: Statement.generator[F, A]): String =
        renderIndent(v.value.getIndent) ++ s"  ${v.value.nme} <- ${renderValue(v.value.impl)}"

      private def renderValDef[T](d: Statement.`val`[T]): String =
        d.value match
          case `Value.Var.Unbound.Data`(in, nme, tpe) => renderIndent(in) ++ s"val ${nme}: ${renderTypeExpr(tpe)}"
          case `Value.Var.Bound.Data`(in, nme, tpe, impl) => renderIndent(in) ++ s"val ${nme}: ${renderTypeExpr(tpe)} = ${renderValue(impl)}"
