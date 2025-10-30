package dc10.scala

import cats.data.NonEmptyList
import dc10.{Error, Renderer}

object version:

  given `3.3.7`: Renderer[NonEmptyList, "scala-3.3.7", Statement] =
    new Renderer[NonEmptyList, "scala-3.3.7", Statement]:

      override def render(input: NonEmptyList[Statement]): String =
        input.map(stmt => stmt match
          case d@CaseDef(_)                   => renderCaseExpr(d)
          case d@CaseClassDef(_, _, _)        => renderCaseClassDef(d)
          case d@`DefDef: x`(_)               => renderDefDef(d)
          case d@`DefDef: x_x x`(_)           => renderDefDef(d)
          case d@`DefDef: x_x_x x x`(_)       => renderDefDef(d)
          case d@LibDep(_, _, _)              => renderLibDep(d)
          case d@ObjDef(_)                    => renderObjectDef(d)
          case d@PackageDef(_, _)             => renderPackageDef(d)
          case d@`SealedTraitDef: x`(_)       => renderSealedTraitDef(d)
          case d@`TraitDef: x`(_)             => renderTraitDef(d)
          case d@`TraitDef: x_x`(_, _, _)     => renderTraitDef(d)
          case d@`TypeDef: x`(_)              => renderTypeDef(d)
          case d@`TypeDef: x_x x`(_)          => renderTypeDef(d)
          case d@`ValDef: x`(_)               => renderValDef(d)
          case d@`ValDef: x_x x`(_)           => renderValDef(d)
          case d@`ValDef: x_x llx_xl_x_x x_x xl`(_)           => renderValDef(d)
          case d@`ValDef: x_x_x x x`(_)                       => renderValDef(d)
          case d@`ValDef: lx_xl_x_x x_x x`(_)                 => renderValDef(d)
          case d@`ValDef: x_x_x x llx_xl_x_x x_x xl`(_)       => renderValDef(d)
          case d@`ValDef: x_x_x x lx_x llx_xl_x_x x_x xll`(_) => renderValDef(d)
          case d@`ValDef: lx_xl_x_x x_x llx_xl_x x_xl`(_)     => renderValDef(d)
          case d@`GivenDef: x`(_)     => renderGivenDef(d)
          case d@`GivenDef: x_x x`(_) => renderGivenDef(d)
        )
        .toList
        .mkString

      override def renderErrors(errors: List[Error]): String =
        errors.map(_.toString()).toList.mkString("\n")

      override def version: "scala-3.3.7" =
        "scala-3.3.7"

      private def renderImpl(i: Option[Value]): String =
        i.fold("")(v => s" = ${renderValue(v)}")

      private def renderIndent(i: Int): String =
        "  ".repeat(i)

      private def renderCaseExpr[T](c: CaseDef[T]): String =
        renderIndent(c.lambda.lvl) ++ s"case ${renderPattern(c.lambda)}\n"

      private def renderCaseClassDef[T](cls: CaseClassDef[T]): String =
        renderIndent(cls.tpe.lvl) ++ s"case class ${cls.tpe}(${
          if cls.fields.length <= 1
          then ??? // render(cls.fields)
          else ??? // "\n" ++ cls.fields.map(f => renderIndent(f.getIndent) ++ render(List(f))).mkString(",\n") ++ "\n"
        })\n"

      // private def renderExtensionDef[T](ext: `extension`[T]): String =
      //   ???
      //   // renderIndent(ext.field.getIndent) ++ s"extension (${ext.field.sym.nme}: ${renderTypeExpr(ext.field.tpe)})\n${render(ext.body)}\n"

      private def renderGivenDef[T](d: `GivenDef: x`[T]): String =
        renderIndent(d.value.lvl) ++ s"given ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderGivenDef[T[_], A](d: `GivenDef: x_x x`[T, A]): String =
        renderIndent(d.value.lvl) ++ s"given ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderLibDep(dep: LibDep): String =
        s"\"${dep.org}\" %% \"${dep.nme}\" % \"${dep.ver}\"\n"

      private def renderObjectDef[T](d: ObjDef[T]): String =
        renderIndent(d.obj.lvl) ++ (
          (d.obj.parent, d.obj.body) match
            case (None, Nil)       => s"object ${d.obj.sym.nme}\n"
            case (Some(p), Nil)    => s"case object ${d.obj.sym.nme} extends ${renderType(p)}\n"
            case (None, h :: t)    => s"object ${d.obj.sym.nme}:\n${render(NonEmptyList(h, t))}\n"
            case (Some(p), h :: t) => s"object ${d.obj.sym.nme} extends ${renderType(p)}:\n${render(NonEmptyList(h, t))}\n"
        )

      private def renderPackageDef(pkg: PackageDef): String =
        if pkg.nme.isEmpty
        then render(pkg.contents)
        else s"package ${pkg.nme.mkString(".")}\n\n${render(pkg.contents)}"

      private def renderPattern(value: Value): String =
        ???
        
      private def renderType[T](tpe: Type): String =
        tpe match
          case `Type.App: x_x x`(_, tfun, targ)                              => s"${renderType(tfun)}[${renderType(targ)}]"
          case `Type.App: x_x llx_xl_x_x x_x xl`(_, tfun, farg)              => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: lx_xl_x x_x`(_, tfun, farg)                        => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: lx_x_xl_x x_x_x`(_, tfun, farg)                  => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: lx_xl_x_x x_x x`(_, tfun, farg, aarg)             => s"${renderType(tfun)}[${renderType(farg)}, ${renderType(aarg)}]"
          case `Type.App: lx_xl_x_x x_x`(_, tfun, farg)                     => s"${renderType(tfun)}[${renderType(farg)}]"
          case `Type.App: x_x_x x x`(_, tfun, ta, tb)                         => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
          // case `Type.App: x_x_x_x x * x`(_, tfun, ta1, ta2, tb)              => s"${renderType(tfun)}[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(tb)}]"
          // case `Type.App: lx_xl_x_x_x lx_xl x x`(_, tfun, f, a, b)         => s"${renderType(tfun)}[${renderType(f)}, ${renderType(a)}, ${renderType(b)}]"
          case `Type.App: lx_xl_x_x x_x llx_xl_x x_xl`(_, tfun, ta, tb)        => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]"
          case `Type.App: lx_xl_x_x x_x llx_xl_x_x x_x xl`(_, tfun, ta, tb)    => s"${renderType(tfun)}[${renderType(ta)}, ${renderType(tb)}]^^"
          case `Type.App: llx_xl_x_xl_x lx_xl_x_x`(_, tfun, arg)         => s"${renderType(tfun)}[${renderType(arg)}]"
          // case `Type.App: x_x_x_x_x x * x x`(_, tfun, a, b, c, d)           => s"${renderType(tfun)}[${renderType(a)}, ${renderType(b)}, ${renderType(c)}, ${renderType(d)}]"
          // case `Type.App: lx_xl_x_x_x_x lx_xl x * x`(_, tfun, f, a, b, c) => s"${renderType(tfun)}[${renderType(f)}, ${renderType(a)}, ${renderType(b)}, ${renderType(c)}]"
          case `Type.App: llx_xl_xl_x lx_xl_x`(_, tfun, targ)              => s"${renderType(tfun)}[${renderType(targ)}]"
          case `Type.AppInfix: x_x_x x x`(_, tfun, ta, tb)                    => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x x lx_x xl`(_, tfun, ta, tb)             => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x x llx_xl_x_x x_x xl`(_, tfun, ta, tb)   => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x x llx_xl_x_x x_xl`(_, tfun, ta, tb)     => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x x lx_x llx_xl_x_x x_x xll`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}%%"
          case `Type.AppInfix: x_x_x lx_x xl llx_xl_x_x x_x xl`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          case `Type.AppInfix: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`(_, tfun, ta, tb) => s"${renderType(ta)} ${renderType(tfun)} ${renderType(tb)}"
          // case `Type.AppInfix: x_x_x_x x * x`(_, tfun, ta, tb, tc)           => s"(${renderType(ta)}, ${renderType(tb)}) ${renderType(tfun)} ${renderType(tc)}"
          // case `Type.AppInfix: x_x_x_x_x x * x x`(_, f, a, b, c, d)         => s"(${renderType(a)}, ${renderType(b)}, ${renderType(c)}) ${renderType(f)} ${renderType(d)}"
          case `Type.Lam: x_x`(_, ta, tb)                                      => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: lx_xl_x_x`(_, ta1, ta2, tb)                        => s"[${renderType(ta1)}, ${renderType(ta2)}] =>> ${renderType(tb)}"
          case `Type.Lam: lx_xl_x`(_, ta, tb)                                 => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: lx_x_xl_x`(_, ta, tb)                              => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lam: x_x_x`(_, ta1, ta2, tb)                             => s"[${renderType(ta1)}, ${renderType(ta2)}] =>> ${renderType(tb)}"
          case `Type.Lam: llx_xl_x_xl_x`(_, ta, tb)                         => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          // case `Type.Lam: x_x_x_x`(_, ta1, ta2, ta3, tb)                     => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}] =>> ${renderType(tb)}"
          // case `Type.Lam: x_x_x_x_x`(_, ta1, ta2, ta3, ta4, tb)             => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}, ${renderType(ta4)}] =>> ${renderType(tb)}"
          // case `Type.Lam: lx_xl_x_x_x`(_, ta1, ta2, ta3, tb)                => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}] =>> ${renderType(tb)}"
          // case `Type.Lam: lx_xl_x_x_x_x`(_, ta1, ta2, ta3, ta4, tb)        => s"[${renderType(ta1)}, ${renderType(ta2)}, ${renderType(ta3)}, ${renderType(ta4)}] =>> ${renderType(tb)}"
          case `Type.Lam: llx_xl_xl_x`(_, ta, tb)                            => s"[${renderType(ta)}] =>> ${renderType(tb)}"
          case `Type.Lit: x`(_, s)                                              => s
          case `Type.Var: x`(_, s, _)                                           => s.nme
          case `Type.Var: x_x x`(_, s, _, _)                                   => s.nme
          case `Type.Var: lx_xl_x x_x`(_, s, _, _)                         => s.nme
          case `Type.Var: lx_xl_x_x x_x x`(_, s, _, _, _)             => s.nme
          case `Type.Var: x_x_x x x`(_, s, _, _, _)                     => s.nme
          case `Type.Var: x_x_x`(_, s, _)                                      => s.nme
          // case `Type.Var: x_x_x_x`(_, s, i)                                  => s.nme
          case `Type.Var: lx_xl_x_x`(_, s, _)                                => s.nme
          // case `Type.Var: lx_xl_x_x_x`(_, s, i)                             => s.nme
          case `Type.Var: llx_xl_x_xl_x`(_, s, _)                           => s.nme
          // case `Type.Var: x_x_x_x_x`(_, s, i)                               => s.nme
          case `Type.Var: lx_x_xl_x x_x_x`(_, s, _, _)                     => s.nme
          case `Type.Var: llx_xl_xl_x lx_xl_x`(_, s, _, _)                 => s.nme
          case `Type.Var: llx_xl_x_xl_x lx_xl_x_x`(_, s, _, _)           => s.nme
          // case `Type.Var: x_x_x_x x * x`(_, s, _, _, _, _)                   => s.nme
          // case `Type.Var: lx_xl_x_x_x lx_xl x x`(_, s, _, _, _, _)         => s.nme
          // case `Type.Var: x_x_x_x_x x * x x`(_, s, _, _, _, _, _)           => s.nme
          // case `Type.Var: lx_xl_x_x_x_x lx_xl x * x`(_, s, _, _, _, _, _) => s.nme
          case `Type.Var: x_x`(_, s, _)                                     => s.nme
          case `Type.Var: lx_xl_x_x x_x`(_, s, tfun, targ1, impl)             => s.nme
          case `Type.Var: lx_xl_x`(_, s, _)                                   => s.nme
          case `Type.Var: lx_x_xl_x`(_, s, _)                                => s.nme
          // case `Type.Var: lx_xl_x_x_x_x`(_, s, _)                          => s.nme
          case `Type.Var: llx_xl_xl_x`(_, s, _)                              => s.nme
          

      private def renderSealedTraitDef[T](t: `SealedTraitDef: x`[T]): String =
        renderIndent(t.tpe.lvl) ++ s"sealed trait ${t.tpe.sym.nme}\n".stripMargin

      private def renderTraitDef[T](t: `TraitDef: x`[T]): String =
        ???
        // renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.sym.nme}${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
        //     |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin
      
      private def renderTraitDef[T[_], A](t: `TraitDef: x_x`[T, A]): String =
        renderIndent(t.tpe.lvl) ++ s"trait ${t.tpe.sym.nme}[${t.targ1.sym.nme}]" + NonEmptyList.fromList(t.body).fold("\n")(nel => ":\n" + render(nel) + "\n")
            // |${t.body.map(s => render(List(s))).mkString("\n")}

      // private def renderTraitDef[T[_[_]], F[_]](t: `trait`.`[_[_]]`[T, F]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}[${renderType(t.tparam)}[_]]${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //     |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin

      // private def renderTraitDef[T[_[_], _], F[_], A](t: `trait`.`[_[_], _]`[T, F, A]): String =
      //   renderIndent(t.tpe.lvl) ++ s"""trait ${t.tpe.nme}[${renderType(t.tparamf)}[_], ${renderType(t.tparama)}]${t.parent.fold("")(p => s" extends ${renderType(p)}")}:
      //       |${t.body.map(s => render(List(s))).mkString("\n")}""".stripMargin
        
      private def renderTypeDef[T](t: `TypeDef: x`[T]): String =
        // renderIndent(t.binding.getIndent) //++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")
        renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}\n")

      // private def renderTypeDef[T[_,_], A, B](t: `TypeDef[A, B]`[T, A, B]): String =
      //   renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}")

      private def renderTypeDef[F[_], A](t: `TypeDef: x_x x`[F, A]): String =
        renderIndent(t.tpe.lvl) ++ t.tpe.impl.fold(s"type ${renderType(t.tpe)}")(i => s"type ${renderType(t.tpe)} = ${renderType(i)}\n")

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
          case `Value.App.1: x`(_, f, a, _)                      => s"${renderValue(f)}(${renderValue(a)})"
          // case `Value.App.2: x`(_, f, a, b, _)                   => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)})"
          // case `Value.App.3: x`(_, f, a, b, c, _)                => s"${renderValue(f)}(${renderValue(a)}, ${renderValue(b)}, ${renderValue(c)})"
          case `Value.App.Vargs: x`(_, f, _, as*)                => s"${renderValue(f)}(${as.map(a => renderValue(a)).mkString(", ")})"
          case `Value.AppDot.0: x`(_, f, a, _)                  => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.0: lx_xl_x_x x_x llx_xl_x_x x_x xl`(_, f, a, _)    => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.0: lx_xl_x_x x_x x`(_, f, a, _)    => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.0: lx_xl_x_x x_x`(_, f, _, a, _)   => s"${renderValue(a)}.${renderValue(f)}"
          case `Value.AppDot.1: x`(_, f, a, b, _)               => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: x_x x`(_, f, a, b, _)           => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: x_x llx_xl_x_x x_x xl`(_, f, a, b, _) => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: lx_xl_x_x x_x x`(_, f, a, b, _) => s"${renderType(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1: lx_xl_x_x x_x llx_xl_x x_xl`(_, f, a, b, _) => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDot.1_: lx_xl_x_x x_x llx_xl_x x_xl`(_, f, a, b, _) => s"${renderValue(a)}.${renderValue(f)}(${renderValue(b)})"
          case `Value.AppDotless: x`(_, f, a, b, _)             => s"${renderValue(a)} ${renderValue(f)} ${renderValue(b)}"
          case `Value.AppForComp: x_x x`(_, l, v, _)                => s"\n${renderIndent(_)}for\n${render(l)}\n${renderIndent(_)}yield ${renderValue(v)}"
          // case `Value.App.Match`(_, a, _, l)                  => s"${renderValue(a)} match\n${render(l)}"
          case `Value.App.1: lx_xl_x_x x_x`(_, f, u, a, _)      => s"${renderValue(f)}[${renderType(u)}](${renderValue(a)})"
          case `Value.App.0: x_x`(_, f, a, _)                => s"${renderValue(f)}[${renderType(a)}]"
          case `Value.App.0: lx_xl_x_x`(_, f, g, a, _)       => s"${renderValue(f)}[${renderType(g)}, ${renderType(a)}]"
          // case `Value.App.0: lx_xl_x_x_x`(_, f, g, a, b, _) => s"${renderValue(f)}[${renderType(g)}, ${renderType(a)}, ${renderType(b)}]"
          case `Value.App.1: x_x x`(_, f, a, _)                => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.App.1: lx_xl_x_x x_x x`(_, f, a, _)      => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.App.1: lx_xl_x_x x_x llx_xl_x x_xl`(_, f, a, _) => s"${renderValue(f)}(${renderValue(a)})"
          case `Value.Lam.1: x_x_x x x`(_, a, b, _)                     => s"${renderValue(a)} => ${renderValue(b)}"
          case `Value.Lam1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(_, a, b, _) => s"${renderValue(a)} => ${renderValue(b)}"
          // case `Value.Lam1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`(l, a, b, t) => s"${renderValue(a)} => ${renderValue(b)}"
          // case `Value.Lam.2: x_x_x_x x * x`(_, a1, a2, r, t)                => s"(${renderValue(a1)}, ${renderValue(a2)}) => ${renderValue(r)}"
          // case `Value.Lam.3: x_x_x_x_x x * x x`(_, a1, a2, a3, r, t)            => s"(${renderValue(a1)}, ${renderValue(a2)}, ${renderValue(a3)}) => ${renderValue(r)}"
          case `Value.Lit.Boolean: x`(_, tpe, b)                => s"$b"
          case `Value.Lit.Int: x`(_, tpe, int)                  => s"$int"
          case `Value.Lit.Long: x`(_, tpe, long)                => s"$long"
          case `Value.Lit.Float: x`(_, tpe, f)                  => s"$f"
          case `Value.Lit.Double: x`(_, tpe, d)                 => s"$d"
          case `Value.Lit.String: x`(_, tpe, s)                 => s"\"$s\""
          case `Value.Lit.Unit: x`(_, tpe, u)                   => s"$u"
          case `Value.Obj: x`(_, s, _, _, _)                   => s.nme
          case `Value.Val: x`(_, s, _, _)                       => s.nme
          case `Value.Val: lx_xl_x`(_, s, _, _)               => s.nme
          case `Value.Val: x_x_x`(_, s, _, _)                 => s.nme
          case `Value.Val: lx_xl_x_x`(_, s, _, _)            => s.nme
          // case `Value.Val: lx_xl_x_x_x`(_, s, _, _)         => s.nme
          case `Value.Val: x_x_x x x`(_, s, _, _)                   => s.nme
          case `Value.Val: x_x_x x llx_xl_x_x x_x xl`(_, s, _, _)   => s.nme
          case `Value.Val: x_x_x x llx_xl_x_x x_xl`(_, s, _, _)     => s.nme
          case `Value.Val: x_x_x x lx_x llx_xl_x_x x_x xll`(_, s, _, _)   => s.nme
          case `Value.Def.1: x_x_x x x`(_, s, _, _)                       => s.nme
          case `Value.Def.0: x`(_, s, _, _)                               => s.nme
          case `Value.Def.0: x_x x`(_, s, _, _)                           => s.nme
          case `Value.Val: x_x x`(_, s, _, _)                             => s.nme
          case `Value.Val: x_x`(_, s, _, _)                               => s.nme
          case `Value.Val: lx_xl_x x_x`(_, s, _, _)                       => s.nme
          case `Value.Val: lx_xl_x_x x_x x`(_, s, _, _)                   => s.nme
          case `Value.Def.1: x_x_x x lx_x xl`(_, s, _, _)                 => s.nme
          case `Value.Def.1: x_x_x x llx_xl_x_x x_x xl`(_, s, _, _)       => s.nme
          case `Value.Def.1: x_x_x x lx_x llx_xl_x_x x_x xll`(_, s, _, _) => s.nme
          case `Value.Def.0: x_x_x lx_x xl llx_xl_x_x x_x xl`(_, s, _, _) => s.nme
          case `Value.Val: lx_xl_x_x x_x`(_, s, _, _, _)                  => s.nme
          case `Value.Val: x_x llx_xl_x_x x_x xl`(_, s, _, _)             => s.nme
          case `Value.Val: lx_xl_x_x x_x llx_xl_x x_xl`(_, s, _, _)       => s.nme
          case `Value.Val: lx_xl_x_x x_x llx_xl_x_x x_x xl`(_, s, _, _)   => s.nme
          case `Value.Val: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(_, s, _, _) => s.nme
          case `Value.Def.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`(_, s, _, _) => s.nme
          case `Value.Def.0: x_x_x lx_x llx_xl_x_x x_x xll llx_xl_x_x x_x llx_xl_x_x x_x xll`(_, s, _, _) => s.nme
          // case `Value.Def.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`(_, s, _, _) => s.nme
          // case `Value.Val: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`(_, s, _, _) => s.nme
          case `Value.Def.1: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xlll llx_xl_x_x x_x llx_xl_x x_xll`(_, s, _, _) => s.nme
          case `Value.Giv: x`(_, s, _, _)     => s.nme
          case `Value.Giv: x_x x`(_, s, _, _) => s.nme
          case `Value.Lit.List: x_x x`(_, _, l) => l.map(v => renderValue(v)).toString

      private def renderValDef[T](d: `ValDef: x`[T]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderDefDef[T](d: `DefDef: x`[T]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderDefDef[T[_], A](d: `DefDef: x_x x`[T, A]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_], A](d: `ValDef: x_x x`[T, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_], G[_[_], _], H[_], A](d: `ValDef: x_x llx_xl_x_x x_x xl`[T, G, H, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_[_], _], F[_], A](d: `ValDef: lx_xl_x_x x_x x`[T, F, A]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_[_], _], F[_], G[_[_]], H[_]](d: `ValDef: lx_xl_x_x x_x llx_xl_x x_xl`[T, F, G, H]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"




      // private def renderValDef[T, A](d: `def`.`0`.`[_]`[T, A]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(_)}"
      //   )

      // private def renderValDef[F[_], A](d: `def`.`0`.`[_[_]]`[F, A]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparam)}[_]]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(_)}"
      //   )

      // private def renderValDef[F[_], T, A](d: `def`.`0`.`[_[_], _]`[F, A, T]): String =
      //   d.impl.fold(
      //     renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)}"
      //   )(
      //     i => renderIndent(d.value.getIndent) ++ s"def ${renderValue(d.value)}[${renderTypeExpr(d.tparamf)}[_], ${renderTypeExpr(d.tparama)}]: ${renderTypeExpr(d.value.tpe)} = ${renderValue(_)}"
      //   )

      private def renderDefDef[A, B](d: `DefDef: x_x_x x x`[A, B]): String =
        renderIndent(d.value.lvl) ++ s"def ${renderValue(d.value)}(${renderValue(d.value.sym.arg1)}: ${renderType(d.value.sym.arg1.tpe)}): ${renderType(d.value.tpe.targ2)}" ++ renderImpl(d.value.impl) + "\n"
  
      private def renderValDef[T[_, _], A, B](d: `ValDef: x_x_x x x`[T, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_, _], G[_[_], _], H[_], A, B](d: `ValDef: x_x_x x llx_xl_x_x x_x xl`[T, G, H, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      private def renderValDef[T[_, _], G[_[_], _], H[_], I[_], A, B](d: `ValDef: x_x_x x lx_x llx_xl_x_x x_x xll`[T, G, H, I, A, B]): String =
        renderIndent(d.value.lvl) ++ s"val ${renderValue(d.value)}: ${renderType(d.value.tpe)}" ++ renderImpl(d.value.impl) + "\n"

      // extension (s: String)
      //   def terminate: String =
      //     s"$s\n"