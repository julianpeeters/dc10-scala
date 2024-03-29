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
  def CASECLASS[T, A](name: String, fields: G[ValueExpr[A]])(using sp: SourcePos): F[(TypeExpr[T], ValueExpr[A => T])]
  @scala.annotation.targetName("caseClass2")
  def CASECLASS[T, A, B](name: String, fields: G[(ValueExpr[A], ValueExpr[B])])(using sp: SourcePos): F[(TypeExpr[T], ValueExpr[(A, B) => T])]
  def EXTENSION[T](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): F[ValueExpr[T]]
  def FIELD[T](nme: String, tpe: F[TypeExpr[T]])(using sp: SourcePos): G[ValueExpr[T]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[
    [A] =>> StateT[ErrorF, List[Statement], A],
    [A] =>> StateT[ErrorF, List[Statement.ValueDef], A]
  ]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[T, A](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], ValueExpr[A]]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, List[Statement], (TypeExpr[T], ValueExpr[A => T])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, List[Statement], (List[Statement.ValueDef], ValueExpr[A])](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields))
        t <- StateT.pure(Term.TypeLevel.App.App2(Term.TypeLevel.Lam.Function1Type(), a.value.tpe, c.tpe))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A => T]](ValueExpr(
          Term.ValueLevel.Lam.Lam1(a.value, Term.ValueLevel.App.AppCtor1(c.tpe, a.value), t)
        ))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A => T]](
          a.value match
            case Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl) => Right[List[Error], Statement.ValueExpr[A => T]](ValueExpr[A => T](
              Term.ValueLevel.Var.UserDefinedValue(
                nme = name,
                tpe = Term.TypeLevel.App.App2(
                    Term.TypeLevel.Lam.Function1Type(),
                    a.value.tpe,
                    c.tpe
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
      fields: StateT[ErrorF, List[Statement.ValueDef], (ValueExpr[A], ValueExpr[B])]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, List[Statement], (TypeExpr[T], ValueExpr[(A, B) => T])] =
      for
        (fields, (a, b)) <- StateT.liftF[ErrorF, List[Statement], (List[Statement.ValueDef], (ValueExpr[A], ValueExpr[B]))](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields))
        t <- StateT.pure(Term.TypeLevel.App.App3(Term.TypeLevel.Lam.Function2Type(), a.value.tpe, b.value.tpe, c.tpe))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[(A, B) => T]](ValueExpr(
          Term.ValueLevel.Lam.Lam2(a.value, b.value, Term.ValueLevel.App.AppCtor2(name, c.tpe, a.value, b.value), t)
        ))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[(A, B) => T]](
          (a.value, b.value) match
            case (Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl), Term.ValueLevel.Var.UserDefinedValue(nme2, tpe2, impl2)) =>
              Right[List[Error], Statement.ValueExpr[(A, B) => T]](ValueExpr[(A, B) => T](
                Term.ValueLevel.Var.UserDefinedValue(
                  nme = name,
                  tpe = Term.TypeLevel.App.App3(
                      Term.TypeLevel.Lam.Function2Type(),
                      a.value.tpe,
                      b.value.tpe,
                      c.tpe
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
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(v)

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement.ValueDef], ValueExpr[T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement.ValueDef], TypeExpr[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement.ValueDef], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement.ValueDef]](ctx => ctx.ext(d))
      yield ValueExpr(v)