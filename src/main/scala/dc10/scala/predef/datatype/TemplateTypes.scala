package dc10.scala.predef.datatype

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.Statement
import dc10.scala.Statement.{TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{CaseClass, Term}
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF, IdentifierSymbolExpected}
import org.tpolecat.sourcepos.SourcePos

trait TemplateTypes[F[_], G[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[T, A](name: String, fields: G[ValueExpr[A]])(using sp: SourcePos): F[(TypeExpr[T], ValueExpr[A => T])]
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
        c <- StateT.pure(CaseClass[T](None, name, fields))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[A => T]](ValueExpr(
          Cofree((), Eval.now(Term.ValueLevel.Lam.Lam1(None, a.value, Cofree((), Eval.now(Term.ValueLevel.App.AppCtor1(None, c.tpe, a.value))))))))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A => T]](
          a.value.tail.value match
            case Term.ValueLevel.App.App1(_, _, _, _)        => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.App.AppCtor1(_, _, _)      => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.App.AppVargs(_, _, vargs*) => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.App.Dot1(_, _, _, _)       => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Lam.Lam1(_, _, _)          => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Lam.Lam2(_, _, _, _)       => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.BooleanLiteral(_, _)   => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.IntLiteral(_, _)       => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.StringLiteral(_, _)    => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.ListCtor(_)            => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.OptionCtor(_)          => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.OptionCtor.SomeCtor(_) => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.Println(_, _)          => Left(scala.List(IdentifierSymbolExpected(a.value.tail.value)))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => Right[List[Error], Statement.ValueExpr[A => T]](ValueExpr[A => T](
              Cofree((), Eval.now(Term.ValueLevel.Var.UserDefinedValue(qnt, name, Cofree((), Eval.now(Term.TypeLevel.App2(None, Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))), tpe, c.tpe))), Some(f.value))))))
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
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement.ValueDef], ValueExpr[T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement.ValueDef], TypeExpr[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement.ValueDef], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement.ValueDef]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))