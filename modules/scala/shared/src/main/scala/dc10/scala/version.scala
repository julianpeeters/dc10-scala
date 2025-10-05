package dc10.scala

import cats.data.NonEmptyList
import dc10.{Error, Renderer}

object version:

  given `3.3.6`: Renderer[NonEmptyList, "scala-3.3.6", Statement] =
    new Renderer[NonEmptyList, "scala-3.3.6", Statement]:

      override def render(input: NonEmptyList[Statement]): String =
        input.map(stmt => stmt match
          case d@CaseDef(_)                   => renderCaseExpr(d)
          case d@CaseClassDef(_, _, _)        => renderCaseClassDef(d)
          case d@`DefDef: *`(_)               => renderDefDef(d)
          case d@`DefDef: *→* *`(_)           => renderDefDef(d)
          case d@`DefDef: *→*→* * *`(_)       => renderDefDef(d)
          case d@LibDep(_, _, _)              => renderLibDep(d)
          case d@ObjDef(_)                    => renderObjectDef(d)
          case d@PackageDef(_, _)             => renderPackageDef(d)
          case d@`TypeDef: *`(_)              => renderTypeDef(d)
          case d@`TypeDef: *→* *`(_)          => renderTypeDef(d)
          case d@`ValDef: *`(_)               => renderValDef(d)
          case d@`ValDef: *→* *`(_)           => renderValDef(d)
          case d@`ValDef: *→* ((*→*)→*→* *→* *)`(_)           => renderValDef(d)
          case d@`ValDef: *→*→* * *`(_)                       => renderValDef(d)
          case d@`ValDef: (*→*)→*→* *→* *`(_)                 => renderValDef(d)
          case d@`ValDef: *→*→* * ((*→*)→*→* *→* *)`(_)       => renderValDef(d)
          case d@`ValDef: *→*→* * (*→* ((*→*)→*→* *→* *))`(_) => renderValDef(d)
        )
        .toList
        .mkString("\n")
        .terminate

      override def renderErrors(errors: List[Error]): String =
        errors.map(_.toString()).toList.mkString("\n")

      override def version: "scala-3.3.6" =
        "scala-3.3.6"

      private def renderImpl(i: Option[Value]): String =
        i.fold("")(v => s" = ${renderValue(v)}")

      private def renderIndent(i: Int): String =
        "  ".repeat(i)

      private def renderCaseExpr[T](c: CaseDef[T]): String =
        renderIndent(c.lambda.lvl) ++ s"case ${renderPattern(c.lambda)}"

      private def renderCaseClassDef[T](cls: CaseClassDef[T]): String =
        renderIndent(cls.tpe.lvl) ++ s"case class ${cls.tpe}(${
          if cls.fields.length <= 1
          then ??? // render(cls.fields)
          else ??? // "\n" ++ cls.fields.map(f => renderIndent(f.getIndent) ++ render(List(f))).mkString(",\n") ++ "\n"
        })"

      // private def renderExtensionDef[T](ext: `extension`[T]): String =
      //   ???
      //   // renderIndent(ext.field.getIndent) ++ s"extension (${ext.field.sym.nme}: ${renderTypeExpr(ext.field.tpe)})\n${render(ext.body)}\n"

      private def renderLibDep(dep: LibDep): String =
        s"\"${dep.org}\" %% \"${dep.nme}\" % \"${dep.ver}\""

      private def renderObjectDef[T](d: ObjDef[T]): String =
        renderIndent(d.obj.lvl) ++ (
          (d.obj.parent, d.obj.body) match
            case (None, Nil)       => s"object ${d.obj.sym.nme}"
            case (Some(p), Nil)    => s"object ${d.obj.sym.nme} extends ${renderType(p)}"
            case (None, h :: t)    => s"object ${d.obj.sym.nme}:\n\n${render(NonEmptyList(h, t))}"
            case (Some(p), h :: t) => s"object ${d.obj.sym.nme} extends ${renderType(p)}:\n\n${render(NonEmptyList(h, t))}"
        )

      private def renderPackageDef(pkg: PackageDef): String =
        if pkg.nme.isEmpty
        then render(pkg.contents)
        else s"package ${pkg.nme.mkString(".")}\n\n${render(pkg.contents)}"

      private def renderPattern(value: Value): String =
        ???
        
      private def renderType[T](tpe: Type): String =
        tpe match
          case `Type.App: *→* *`(in, tfun, targ)                              => s"${renderType(tfun)}[${renderType(targ)}]"
          case `Type.App: *→* ((*→*)→*→* *→* *)`(in, tfun, farg)              => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: (*→*)→* *→*`(in, tfun, farg)                        => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: (*→*→*)→* *→*→*`(in, tfun, farg)                  => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: (*→*)→*→* *→* *`(in, tfun, farg, aarg)             => s"${renderType(tfun)}[${renderType(farg)}, ${renderType(aarg)}]"
          case `Type.App: (*→*)→*→* *→*`(in, tfun, farg)                     => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: *→*→* * *`(in, tfun, ta, tb)                         => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
          case `Type.App: *→*→*→* * * *`(in, tfun, ta1, ta2, tb)              => s"${renderType(tfun)}[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(tb)}]"
          case `Type.App: (*→*)→*→*→* (*→*) * *`(in, tfun, f, a, b)         => s"${renderType(tfun)}[${renderType(f)}, ${renderType(a)}, ${renderType(b)}]"
          case `Type.App: ((*→*)→*→*)→* (*→*)→*→*`(in, tfun, arg)         => s"${renderType(tfun)}[${renderType(arg)}]"
          case `Type.App: *→*→*→*→* * * * *`(in, tfun, a, b, c, d)           => s"${renderType(tfun)}[${renderType(a)}, ${renderType(b)}, ${renderType(c)}, ${renderType(d)}]"
          case `Type.App: (*→*)→*→*→*→* (*→*) * * *`(in, tfun, f, a, b, c) => s"${renderType(tfun)}[${renderType(f)}, ${renderType(a)}, ${renderType(b)}, ${renderType(c)}]"
          case `Type.App: ((*→*)→*)→* (*→*)→*`(in, tfun, targ)              => s"${renderType(tfun)}[${renderType(targ)}]"
          case `Type.AppInfix: *→*→* * *`(in, tfun, ta, tb)                    => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: *→*→* * (*→* *)`(in, tfun, ta, tb)             => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`(in, tfun, ta, tb)   => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: *→*→* * ((*→*)→*→* *→*)`(in, tfun, ta, tb)     => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`(in, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}%%"
          case `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`(in, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: *→*→*→* * * *`(in, tfun, ta, tb, tc)           => s"(${renderType(ta)}, ${renderType(tb)}) ${renderType(tfun)} ${renderType(tc)}"
          case `Type.AppInfix: *→*→*→*→* * * * *`(in, f, a, b, c, d)         => s"(${renderType(a)}, ${renderType(b)}, ${renderType(c)}) ${renderType(f)} ${renderType(d)}"
          case `Type.Lam: *→*`(in, ta, tb)                                      => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: (*→*)→*→*`(in, ta1, ta2, tb)                        => s"[${renderType(ta1)}, ${renderType(ta2)}] =>> ${renderType(tb)}"
          case `Type.Lam: (*→*)→*`(in, ta, tb)                                 => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: (*→*→*)→*`(in, ta, tb)                              => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: *→*→*`(in, ta1, ta2, tb)                             => s"[${renderType(ta1)}, ${renderType(ta2)}] =>> ${renderType(tb)}"
          case `Type.Lam: ((*→*)→*→*)→*`(in, ta, tb)                         => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: *→*→*→*`(in, ta1, ta2, ta3, tb)                     => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}] =>> ${renderType(tb)}"
          case `Type.Lam: *→*→*→*→*`(in, ta1, ta2, ta3, ta4, tb)             => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}, ${renderType(ta4)}] =>> ${renderType(tb)}"
          case `Type.Lam: (*→*)→*→*→*`(in, ta1, ta2, ta3, tb)                => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}] =>> ${renderType(tb)}"
          case `Type.Lam: (*→*)→*→*→*→*`(in, ta1, ta2, ta3, ta4, tb)        => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}, ${renderType(ta4)}] =>> ${renderType(tb)}"
          case `Type.Lam: ((*→*)→*)→*`(in, ta, tb)                            => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lit: *`(in, s)                                              => s
          case `Type.Var: *`(in, s, i)                                           => s.nme
          case `Type.Var: *→* *`(in, s, i, c)                                   => s.nme
          case `Type.Var: (*→*)→* *→*`(in, s, arg, i)                         => s.nme
          case `Type.Var: (*→*)→*→* *→* *`(in, s, arg1, arg2, i)             => s.nme
          case `Type.Var: *→*→* * *`(in, s, aarg, barg, i)                     => s.nme
          case `Type.Var: *→*→*`(l, s, i)                                      => s.nme
          case `Type.Var: *→*→*→*`(in, s, i)                                  => s.nme
          case `Type.Var: (*→*)→*→*`(in, s, i)                                => s.nme
          case `Type.Var: (*→*)→*→*→*`(in, s, i)                             => s.nme
          case `Type.Var: ((*→*)→*→*)→*`(in, s, i)                           => s.nme
          case `Type.Var: *→*→*→*→*`(in, s, i)                               => s.nme
          case `Type.Var: (*→*→*)→* *→*→*`(in, s, _, _)                     => s.nme
          case `Type.Var: ((*→*)→*)→* (*→*)→*`(in, s, _, _)                 => s.nme
          case `Type.Var: ((*→*)→*→*)→* (*→*)→*→*`(in, s, _, _)           => s.nme
          case `Type.Var: *→*→*→* * * *`(in, s, _, _, _, _)                   => s.nme
          case `Type.Var: (*→*)→*→*→* (*→*) * *`(in, s, _, _, _, _)         => s.nme
          case `Type.Var: *→*→*→*→* * * * *`(in, s, _, _, _, _, _)           => s.nme
          case `Type.Var: (*→*)→*→*→*→* (*→*) * * *`(in, s, _, _, _, _, _) => s.nme
          case `Type.Var: *→*`(in, s, i, _)                                     => s.nme
          case `Type.Var: (*→*)→*→* *→*`(in, s, tfun, targ1, impl)             => s.nme
          case `Type.Var: (*→*)→*`(in, s, _)                                   => s.nme
          case `Type.Var: (*→*→*)→*`(in, s, _)                                => s.nme
          case `Type.Var: (*→*)→*→*→*→*`(in, s, _)                          => s.nme
          case `Type.Var: ((*→*)→*)→*`(in, s, _)                              => s.nme
          

      // private def renderTraitDef[T](t: `trait`[T]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //       |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      // private def renderTraitDef[T[_], A](t: `trait`.`[_]`[T, A]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}[${renderType(t.tparam)}]${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //       |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      // private def renderTraitDef[T[_[_]], F[_]](t: `trait`.`[_[_]]`[T, F]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}[${renderType(t.tparam)}[_]]${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //     |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      // private def renderTraitDef[T[_[_], _], F[_], A](t: `trait`.`[_[_], _]`[T, F, A]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}[${renderType(t.tparamf)}[_], ${renderType(t.tparama)}]${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //       |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin
        
      private def renderTypeDef[T](t: `TypeDef: *`[T]): String =
        // renderIndent(t.binding.getIndent) //++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")
        renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")

      // private def renderTypeDef[T[_,_], A, B](t: `TypeDef[A, B]`[T, A, B]): String =
      //   renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")

      private def renderTypeDef[F[_], A](t: `TypeDef: *→* *`[F, A]): String =
        renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")

      // private def renderTypeDef[T[_[_]], F[_]](t: `TypeDef[F[_]]`[T, F]): String =
      //   renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")

      // private def renderTypeDef[T[_[_[_]]], F[_[_]]](t: `TypeDef[F[_[_]]]`[T, F]): String =
      //   ???
        // renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}[${renderType(t.tparam)}]")(i => s"type ${renderType(t.tpe)}[${renderType(t.tparam)}] = ${renderType(i)}")
     
      // private def renderTypeDef[T[_[_]], F[_]](t: `type`.`[_[_]]`[T, F]): String =
      //   renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}[_]]")(i => s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparam)}[_]] = ${renderTypeExpr(i)}")

      // private def renderTypeDef[T[_[_], _], F[_], A](t: `type`.`[_[_], _]`[T, F, A]): String =
      //   renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparamf)}[_], ${renderTypeExpr(t.tparama)}]")(i => s"type ${renderTypeExpr(t.tpe)}[${renderTypeExpr(t.tparamf)}[_], ${renderTypeExpr(t.tparama)}] = ${renderTypeExpr(i)}")

      // private def renderParams(sym: DefSym): String =
      //   sym match
      //     case `DefSym.0`(nme) => s""
      //     case `DefSym.1`(nme, arg1) => s"(${renderValue(arg1)}: ${renderType(arg1.tpe)})"
        
      private def renderValue(value: Value): String =
        value match 
          case `Value.App.1: *`(i, f, a, t)                      => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.App.2: *`(i, f, a, b, t)                   => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)})"
          case `Value.App.3: *`(i, f, a, b, c, t)                => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)}, ${renderValue(c)})"
          case `Value.App.Vargs: *`(i, f, t, as*)                => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case `Value.AppDot.0: *`(i, f, a, t)                  => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.0: (*→*)→*→* *→* *`(i, f, a, t)    => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.0: (*→*)→*→* *→*`(l, f, t, a, i)   => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.1: *`(i, f, a, b, t)               => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: *→* *`(i, f, a, b, t)           => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: *→* ((*→*)→*→* *→* *)`(i, f, a, b, t) => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: (*→*)→*→* *→* *`(i, f, a, b, t)    => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDotless: *`(i, f, a, b, t)             => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          case `Value.AppForComp: *→* *`(i, l, v, t)                => s"\n${renderIndent(i)}for\n${render(l)}\n${renderIndent(i)}yield ${renderValue(v)}"
          // case `Value.App.Match`(i, a, t, l)                  => s"${renderValue(a)} match\n${render(l)}"
          case `Value.App.1: (*→*)→*→* *→*`(i, f, u, a, t)      => s"${renderValue(f)}[${renderType(u)}](${renderValue(a)})"
          case `Value.App.0: *→*`(i, f, a, t)                => s"${renderValue(f)}[${renderType(a)}]"
          case `Value.App.0: (*→*)→*→*`(i, f, g, a, t)       => s"${renderValue(f)}[${renderType(g)}, ${renderType(a)}]"
          case `Value.App.0: (*→*)→*→*→*`(i, f, g, a, b, t) => s"${renderValue(f)}[${renderType(g)}, ${renderType(a)}, ${renderType(b)}]"
          case `Value.App.1: *→* *`(l, f, a, t)                => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.App.1: (*→*)→*→* *→* *`(l, f, a, t)      => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.Lam.1: *→*→* * *`(i, a, b, t)                     => s"${renderValue(a)} => ${renderValue(b)}"
          case `Value.Lam.2: *→*→*→* * * *`(i, a1, a2, r, t)                => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(r)}"
          case `Value.Lam.3: *→*→*→*→* * * * *`(i, a1, a2, a3, r, t)            => s"(${renderValue(a1)}, ${renderValue(a2)}, ${renderValue(a3)}) => ${renderValue(r)}"
          case `Value.Lit.Boolean: *`(i, tpe, b)                => s"$b"
          case `Value.Lit.Int: *`(i, tpe, int)                  => s"$int"
          case `Value.Lit.Long: *`(i, tpe, long)                => s"$long"
          case `Value.Lit.Float: *`(i, tpe, f)                  => s"$f"
          case `Value.Lit.Double: *`(i, tpe, d)                 => s"$d"
          case `Value.Lit.String: *`(i, tpe, s)                 => s"\"$s\""
          case `Value.Lit.Unit: *`(i, tpe, u)                   => s"$u"
          case `Value.Obj: *`(in, s, _, _, _)                   => s.nme
          case `Value.Val: *`(l, s, t, i)                       => s.nme
          case `Value.Val: (*→*)→*`(l, s, t, _)               => s.nme
          case `Value.Val: *→*→*`(l, s, t, _)                 => s.nme
          case `Value.Val: (*→*)→*→*`(l, s, t, i)            => s.nme
          case `Value.Val: (*→*)→*→*→*`(l, s, t, _)         => s.nme
          case `Value.Val: *→*→* * *`(l, s, r, i)                   => s.nme
          case `Value.Val: *→*→* * ((*→*)→*→* *→* *)`(l, s, a, i)   => s.nme
          case `Value.Val: *→*→* * ((*→*)→*→* *→*)`(l, s, t, i)     => s.nme
          case `Value.Val: *→*→* * (*→* ((*→*)→*→* *→* *))`(l, s, t, i)   => s.nme
          case `Value.Def.1: *→*→* * *`(l, s, a, i)                       => s.nme
          case `Value.Def.0: *`(l, s, a, i)                               => s.nme
          case `Value.Def.0: *→* *`(l, s, a, i)                           => s.nme
          case `Value.Val: *→* *`(l, s, a, i)                             => s.nme
          case `Value.Val: *→*`(l, s, a, i)                               => s.nme
          case `Value.Val: (*→*)→*→* *→* *`(l, s, a, i)                   => s.nme
          case `Value.Def.1: *→*→* * (*→* *)`(l, s, a, i)                 => s.nme
          case `Value.Def.1: *→*→* * ((*→*)→*→* *→* *)`(l, s, a, i)       => s.nme
          case `Value.Def.1: *→*→* * (*→* ((*→*)→*→* *→* *))`(l, s, a, i) => s.nme
          case `Value.Def.0: *→*→* (*→* *) ((*→*)→*→* *→* *)`(l, s, a, i) => s.nme
          case `Value.Val: (*→*)→*→* *→*`(l, s, t, a, i)                  => s.nme
          case `Value.Val: *→* ((*→*)→*→* *→* *)`(l, s, a, i)             => s.nme

      private def renderValDef[T](d: `ValDef: *`[T]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderDefDef[T](d: `DefDef: *`[T]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderDefDef[T[_], A](d: `DefDef: *→* *`[T, A]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderValDef[T[_], A](d: `ValDef: *→* *`[T, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderValDef[T[_], G[_[_], _], H[_], A](d: `ValDef: *→* ((*→*)→*→* *→* *)`[T, G, H, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderValDef[T[_[_], _], F[_], A](d: `ValDef: (*→*)→*→* *→* *`[T, F, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)




      // private def renderValDef[T, A](d: `def`.`0`.`[_]`[T, A]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
      //   )

      // private def renderValDef[F[_], A](d: `def`.`0`.`[_[_]]`[F, A]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
      //   )

      // private def renderValDef[F[_], T, A](d: `def`.`0`.`[_[_], _]`[F, A, T]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(i)}"
      //   )

      private def renderDefDef[A, B](d: `DefDef: *→*→* * *`[A, B]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}(${renderValue(d.value.sym.arg1)}: ${renderType(d.value.sym.arg1.tpe)}): ${renderType(d.value.tpe.targ2)}" ++ renderImpl(d.value.impl)
  
      private def renderValDef[T[_, _], A, B](d: `ValDef: *→*→* * *`[T, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderValDef[T[_, _], G[_[_], _], H[_], A, B](d: `ValDef: *→*→* * ((*→*)→*→* *→* *)`[T, G, H, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      private def renderValDef[T[_, _], G[_[_], _], H[_], I[_], A, B](d: `ValDef: *→*→* * (*→* ((*→*)→*→* *→* *))`[T, G, H, I, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl)

      extension (s: String)
        def terminate: String =
          s"$s\n"