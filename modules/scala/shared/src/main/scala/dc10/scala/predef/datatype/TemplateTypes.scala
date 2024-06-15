package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.ctx.{dep, ext}
import dc10.scala.{Error, ErrorF, LibDep, Statement}
import dc10.scala.Statement.{TraitDef, ValueDef}
import dc10.scala.Statement.TraitDef.{`trait`, `trait[_[_]]`}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_]]`, `Type[_[_], _]`}
import dc10.scala.Statement.ValueExpr.`Value`
import dc10.scala.Symbol.{CaseClass, Term, Trait}
import org.tpolecat.sourcepos.SourcePos

trait TemplateTypes[F[_], G[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[T, A](name: String, fields: G[`Value`[A]])(using sp: SourcePos): F[(`Type`[T], `Value`[A => T])]
  @scala.annotation.targetName("caseClass2")
  def CASECLASS[T, A, B](name: String, fields: G[(`Value`[A], `Value`[B])])(using sp: SourcePos): F[(`Type`[T], `Value`[(A, B) => T])]
  def EXTENSION[T](nme: String, tpe: F[`Type`[T]])(using sp: SourcePos): F[`Value`[T]]
  def FIELD[T](nme: String, tpe: F[`Type`[T]])(using sp: SourcePos): G[`Value`[T]]
  @scala.annotation.targetName("trait*")
  def TRAIT[T](nme: String, members: F[Unit])(using sp: SourcePos): F[`Type`[T]]
  @scala.annotation.targetName("trait*->*")
  def TRAIT[T[_], A](nme: String, tparam: F[`Type`[A]], members: `Type`[A] => F[Unit])(using sp: SourcePos): F[`Type[_]`[T]]
  @scala.annotation.targetName("trait(*->*)->*")
  def TRAIT[T[_[_]], H[_]](nme: String, tparam: F[`Type[_]`[H]], members: `Type[_]`[H] => F[Unit])(using sp: SourcePos): F[`Type[_[_]]`[T]]
  @scala.annotation.targetName("trait(*->*)->*->*")
  def TRAIT[T[_[_], _], H[_], A](nme: String, tparamF: F[`Type[_]`[H]], tparamA: F[`Type`[A]], members: (`Type[_]`[H], `Type`[A]) => F[Unit])(using sp: SourcePos): F[`Type[_[_], _]`[T]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[
    [A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A],
    [A] =>> StateT[ErrorF, List[Statement.ValueDef], A]
  ]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[T, A](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], `Value`[A]]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Type[T], `Value`[A => T])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (List[Statement.ValueDef], `Value`[A])](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields, Nil))
        n <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType`[T](name, None))
        v <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value`[A => T]](
          a.value match
            case Term.ValueLevel.Var.UserDefinedValue(nme, tpe, mpl) => Right[List[Error], Statement.ValueExpr.`Value`[A => T]](`Value`[A => T](
              Term.ValueLevel.Var.UserDefinedValue(
                nme = name,
                tpe = Term.TypeLevel.App.`App[_, _]`(Term.TypeLevel.Var.`UserDefinedType[_, _]`("=>", None), a.value.tpe, n),
                impl = None
              )
            ))
            case _ => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value}")))
          )
        d <- StateT.pure(Statement.`case class`(0, sp, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (Type(n), v)

    @scala.annotation.targetName("caseClass2")
    def CASECLASS[T, A, B](
      name: String,
      fields: StateT[ErrorF, List[Statement.ValueDef], (`Value`[A], `Value`[B])]
    )(
      using
        sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Type[T], `Value`[(A, B) => T])] =
      for
        (fields, (a, b)) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (List[Statement.ValueDef], (`Value`[A], `Value`[B]))](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields, Nil))
        n <- StateT.pure(Term.TypeLevel.Var.`UserDefinedType`[T](name, None))
        v <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value`[(A, B) => T]](
          (a.value, b.value) match
            case (Term.ValueLevel.Var.UserDefinedValue(nme, tpe, impl), Term.ValueLevel.Var.UserDefinedValue(nme2, tpe2, impl2)) =>
              Right[List[Error], Statement.ValueExpr.`Value`[(A, B) => T]](`Value`[(A, B) => T](
                Term.ValueLevel.Var.UserDefinedValue(
                  nme = name,
                  tpe = Term.TypeLevel.App.`App[_, _, _]`(
                      Term.TypeLevel.Var.`UserDefinedType[_, _, _]`("=>", None),
                      a.value.tpe,
                      b.value.tpe,
                      n
                  ),
                  impl = None
                )
              ))
            case _ => Left(scala.List(Error(s"${sp.file}:${sp.line}\nExpected Identifier but found ${a.value}")))

          )
        d <- StateT.pure(Statement.`case class`(0, sp, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (Type(n), v)

    def EXTENSION[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]] =
      for
        t <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Type[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.Fld(0, sp, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(v)

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, List[Statement.ValueDef], `Value`[T]] =
      for
        t <- StateT.liftF[ErrorF, List[Statement.ValueDef], Type[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, None))
        d <- StateT.pure[ErrorF, List[Statement.ValueDef], ValueDef](ValueDef.Fld(0, sp, v))
        _ <- StateT.modifyF[ErrorF, List[Statement.ValueDef]](ctx => ctx.ext(d))
      yield Value(v)  
  
    @scala.annotation.targetName("trait*")
    def TRAIT[T](
      nme: String,
      members: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
      for
        members <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members.runEmptyS)
        c <- StateT.pure(Trait.`*`[T](nme, members._2))
        d <- StateT.pure(TraitDef.`trait`(0, sp, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type`(Term.TypeLevel.Var.`UserDefinedType`(nme, None))

    @scala.annotation.targetName("trait*->*")
    def TRAIT[T[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      members: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    )(using sp: SourcePos): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        c <- StateT.pure(Trait.`*->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_]`(0, sp, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`(nme, None))

    @scala.annotation.targetName("trait(*->*)->*")
    def TRAIT[T[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      members: `Type[_]`[H] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        c <- StateT.pure(Trait.`(*->*)->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_[_]]`(0, sp, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_[_]]`(Term.TypeLevel.Var.`UserDefinedType[_[_]]`(nme, None))

    @scala.annotation.targetName("trait(*->*)->*->*")
    def TRAIT[T[_[_], _], H[_], A](
      nme: String,
      tparamF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      tparamA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      members: (`Type[_]`[H], `Type`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    )(
      using sp: SourcePos
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[T]] =
      for
        f <- StateT.liftF(tparamF.runEmptyA)
        a <- StateT.liftF(tparamA.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(f, a).runEmptyS)
        c <- StateT.pure(Trait.`(*->*)->*->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_[_], _]`(0, sp, f.tpe, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_[_], _]`(Term.TypeLevel.Var.`UserDefinedType[_[_], _]`(nme, None))