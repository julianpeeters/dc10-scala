package dc10.scala.predef.datatype

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.given
import dc10.scala.Statement
import dc10.scala.Statement.{TypeExpr, ValueDef, ValueExpr}
import dc10.scala.Symbol.{CaseClass, Term}
import dc10.scala.ctx.ext
import dc10.scala.{Error, ErrorF}
import org.tpolecat.sourcepos.SourcePos

trait TemplateTypes[F[_], G[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[Z, T, A](name: String, fields: G[ValueExpr[Z, A]])(using sp: SourcePos): F[(TypeExpr[Unit, T], ValueExpr[Unit, A => T])]
  def EXTENSION[T](nme: String, tpe: F[TypeExpr[Unit, T]])(using sp: SourcePos): F[ValueExpr[Unit, T]]
  def FIELD[T](nme: String, tpe: F[TypeExpr[Unit, T]])(using sp: SourcePos): G[ValueExpr[Unit, T]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[
    [A] =>> StateT[ErrorF, List[Statement], A],
    [A] =>> StateT[ErrorF, List[Statement.ValueDef], A]
  ]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[Z, T, A](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], ValueExpr[Z, A]]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, List[Statement], (TypeExpr[Unit, T], ValueExpr[Unit, A => T])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, List[Statement], (List[Statement.ValueDef], ValueExpr[Z, A])](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](None, name, fields))
        f <- StateT.pure[ErrorF, List[Statement], ValueExpr[Z, A => T]](ValueExpr(
          Cofree(a.value.head, Eval.now(Term.ValueLevel.Lam.Lam1(
            None,
            a.value,
            // Cofree(a.value.head, Eval.now(Term.ValueLevel.App.AppCtor1(None, c.tpe, a.value.head)))
            Cofree(a.value.head, Eval.now(Term.ValueLevel.App.AppCtor1(
              None,
              c.tpe,
              a.value.map(_=> ())
            )))
          )))))
        v <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Unit, A => T]](
          a.value.tail.value match
            case Term.ValueLevel.App.App1(_, _, _, _)          => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.App.AppCtor1(_, _, _)         => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.App.AppPure(_, _, _, _)       => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.App.AppVargs(_, _, _, vargs*) => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.App.Dot1(_, _, _, _)          => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.App.Dotless(_, _, _, _)       => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Lam.Lam1(_, _, _)             => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Lam.Lam2(_, _, _, _)          => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.BooleanLiteral(_, _)      => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.IntLiteral(_, _)          => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.StringLiteral(_, _)       => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.ListCtor(_)               => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.OptionCtor(_)             => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.OptionCtor.SomeCtor(_)    => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.Println(_, _)             => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value.tail.value}")))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => Right[List[Error], Statement.ValueExpr[Unit, A => T]](ValueExpr[Unit, A => T](
              Cofree((), Eval.now(Term.ValueLevel.Var.UserDefinedValue(qnt, name, Cofree((), Eval.now(Term.TypeLevel.App2(None, Cofree((), Eval.now(Term.TypeLevel.Var.Function1Type(None))), tpe.map(_ => ()), c.tpe))), Some(f.value.map(_ => ())))))))
        )
        d <- StateT.pure(Statement.CaseClassDef(c, 0))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield (TypeExpr(c.tpe), v)

    def EXTENSION[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Unit, T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement], ValueExpr[Unit, T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement], TypeExpr[Unit, T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, List[Statement], TypeExpr[Unit, T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement.ValueDef], ValueExpr[Unit, T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement.ValueDef], TypeExpr[Unit, T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(None, nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement.ValueDef], ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, List[Statement.ValueDef]](ctx => ctx.ext(d))
      yield ValueExpr(Cofree((), Eval.now(v)))