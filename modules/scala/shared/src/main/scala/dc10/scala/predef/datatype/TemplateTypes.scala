package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF}
import dc10.scala.Statement
import dc10.scala.Statement.{TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{CaseClass, Term}
import org.tpolecat.sourcepos.SourcePos

trait TemplateTypes[F[_], G[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[T, A](name: String, fields: G[ValueExpr[A, Unit]])(using sp: SourcePos): F[(TypeExpr[T, Unit], ValueExpr[A => T, Unit])]
  @scala.annotation.targetName("caseClass2")
  def CASECLASS[T, A, B](name: String, fields: G[(ValueExpr[A, Unit], ValueExpr[B, Unit])])(using sp: SourcePos): F[(TypeExpr[T, Unit], ValueExpr[(A, B) => T, Unit])]
  def EXTENSION[T](nme: String, tpe: F[TypeExpr[T, Unit]])(using sp: SourcePos): F[ValueExpr[T, Unit]]
  def FIELD[T](nme: String, tpe: F[TypeExpr[T, Unit]])(using sp: SourcePos): G[ValueExpr[T, Unit]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[
    [A] =>> StateT[ErrorF, List[Statement], A],
    [A] =>> StateT[ErrorF, List[Statement.ValueDef], A]
  ]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[T, A](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], ValueExpr[A, Unit]]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, List[Statement], (TypeExpr[T, Unit], ValueExpr[A => T, Unit])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, List[Statement], (List[Statement.ValueDef], ValueExpr[A, Unit])](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](None, name, fields))
        t <- StateT.pure(Term.TypeLevel.App.App2(None, Term.TypeLevel.Lam.Function1Type(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), a.value.tpe, c.tpe, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A => T, Unit]](ValueExpr(
          Term.ValueLevel.Lam.Lam1(None, a.value, Term.ValueLevel.App.AppCtor1(None, c.tpe, a.value), t)
        ))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A => T, Unit]](
          a.value match
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => Right[List[Error], Statement.ValueExpr[A => T, Unit]](ValueExpr[A => T, Unit](
              Term.ValueLevel.Var.UserDefinedValue(
                qnt,
                nme = name,
                tpe = Term.TypeLevel.App.App2(
                    None,
                    Term.TypeLevel.Lam.Function1Type(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                    a.value.tpe,
                    c.tpe,
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())
                ),
                impl = Some(f.value))
            ))
            case _ => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value}")))
          )
        d <- StateT.pure(Statement.CaseClassDef(c, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield (TypeExpr(c.tpe), v)

    @scala.annotation.targetName("caseClass2")
    def CASECLASS[T, A, B](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], (ValueExpr[A, Unit], ValueExpr[B, Unit])]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, List[Statement], (TypeExpr[T, Unit], ValueExpr[(A, B) => T, Unit])] =
      for
        (fields, (a, b)) <- StateT.liftF[ErrorF, List[Statement], (List[Statement.ValueDef], (ValueExpr[A, Unit], ValueExpr[B, Unit]))](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](None, name, fields))
        t <- StateT.pure(Term.TypeLevel.App.App3(None, Term.TypeLevel.Lam.Function2Type(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), a.value.tpe, b.value.tpe, c.tpe, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[(A, B) => T, Unit]](ValueExpr(
          Term.ValueLevel.Lam.Lam2(None, a.value, b.value, Term.ValueLevel.App.AppCtor2(None, name, c.tpe, a.value, b.value), t)
        ))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[(A, B) => T, Unit]](
          (a.value, b.value) match
            case (Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl), Term.ValueLevel.Var.UserDefinedValue(qnt2, nme2, tpe2, impl2)) =>
              Right[List[Error], Statement.ValueExpr[(A, B) => T, Unit]](ValueExpr[(A, B) => T, Unit](
                Term.ValueLevel.Var.UserDefinedValue(
                  qnt,
                  nme = name,
                  tpe = Term.TypeLevel.App.App3(
                      None,
                      Term.TypeLevel.Lam.Function2Type(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                      a.value.tpe,
                      b.value.tpe,
                      c.tpe,
                      Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())
                  ),
                  impl = Some(f.value)
                )
              ))
            case _ => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value}")))

          )
        d <- StateT.pure(Statement.CaseClassDef(c, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield (TypeExpr(c.tpe), v)

    def EXTENSION[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Unit]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[T, Unit]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[T, Unit]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T, Unit]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement.ValueDef], ValueExpr[T, Unit]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement.ValueDef], TypeExpr[T, Unit]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement.ValueDef], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement.ValueDef]](ctx => ctx.ext(d))
      yield ValueExpr(v)