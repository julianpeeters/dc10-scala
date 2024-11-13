package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{Error, ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.{TraitDef, ValueDef}
// import dc10.scala.Statement.TraitDef.{`trait`, `trait[_[_]]`}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_]]`, `Type[_[_], _]`}
import dc10.scala.Statement.ValueExpr.`Value`
import dc10.scala.Symbol.{CaseClass, Term, Trait}

trait TemplateTypes[F[_]]:
  @scala.annotation.targetName("caseClass1")
  def CASECLASS[T, A](name: String, fields: F[`Value`[A]]): F[(`Type`[T], `Value`[A => T])]
  @scala.annotation.targetName("caseClass2")
  def CASECLASS[T, A, B](name: String, fields: F[(`Value`[A], `Value`[B])]): F[(`Type`[T], `Value`[(A, B) => T])]
  def FIELD[T](nme: String, tpe: F[`Type`[T]]): F[`Value`[T]]
  @scala.annotation.targetName("trait*")
  def TRAIT[T](nme: String, members: F[Unit]): F[`Type`[T]]
  @scala.annotation.targetName("trait*->*")
  def TRAIT[T[_], A](nme: String, tparam: F[`Type`[A]], members: `Type`[A] => F[Unit]): F[`Type[_]`[T]]
  @scala.annotation.targetName("trait(*->*)->*")
  def TRAIT[T[_[_]], H[_]](nme: String, tparam: F[`Type[_]`[H]], members: `Type[_]`[H] => F[Unit]): F[`Type[_[_]]`[T]]
  @scala.annotation.targetName("trait(*->*)->*->*")
  def TRAIT[T[_[_], _], H[_], A](nme: String, tparamF: F[`Type[_]`[H]], tparamA: F[`Type`[A]], members: (`Type[_]`[H], `Type`[A]) => F[Unit]): F[`Type[_[_], _]`[T]]

object TemplateTypes:

  trait Mixins extends TemplateTypes[
    StateT[ErrorF, (Set[LibDep], List[Statement]), _],
  ]:
 
    @scala.annotation.targetName("caseClass1")
    def CASECLASS[T, A](
      name: String,
      fields: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Type[T], `Value`[A => T])] =
      for
        (fields, a) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), ((Set[LibDep], List[Statement]), `Value`[A])](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields._2, Nil))
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
            case _ => Left(List(Error(s"Expected Identifier but found ${a.value}")))
          )
        d <- StateT.pure(Statement.`case class`(0, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (Type(n), v)

    @scala.annotation.targetName("caseClass2")
    def CASECLASS[T, A, B](
      name: String,
      fields: StateT[ErrorF, (Set[LibDep], List[Statement]), (`Value`[A], `Value`[B])]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), (Type[T], `Value`[(A, B) => T])] =
      for
        (fields, (a, b)) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), ((Set[LibDep], List[Statement]), (`Value`[A], `Value`[B]))](fields.runEmpty)
        c <- StateT.pure(CaseClass[T](name, fields._2, Nil))
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
            case _ => Left(List(Error(s"Expected Identifier but found ${a.value}")))
          )
        d <- StateT.pure(Statement.`case class`(0, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield (Type(n), v)

    def FIELD[T](
      nme: String,
      tpe: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]] =
      for
        t <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Type[T]](tpe.runEmptyA)
        v <- StateT.pure(Term.ValueLevel.Var.UserDefinedValue(nme, t.tpe, None))
        d <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), ValueDef](ValueDef.Fld(0, v))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield Value(v)  
  
    @scala.annotation.targetName("trait*")
    def TRAIT[T](
      nme: String,
      members: StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), Type[T]] =
      for
        members <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members.runEmptyS)
        c <- StateT.pure(Trait.`*`[T](nme, members._2))
        d <- StateT.pure(TraitDef.`trait`(0, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
      yield `Type`(Term.TypeLevel.Var.`UserDefinedType`(nme, None))

    @scala.annotation.targetName("trait*->*")
    def TRAIT[T[_], A](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      members: `Type`[A] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        c <- StateT.pure(Trait.`*->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_]`(0, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`(nme, None))

    @scala.annotation.targetName("trait(*->*)->*")
    def TRAIT[T[_[_]], H[_]](
      nme: String,
      tparam: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      members: `Type[_]`[H] => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[T]] =
      for
        a <- StateT.liftF(tparam.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(a).runEmptyS)
        c <- StateT.pure(Trait.`(*->*)->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_[_]]`(0, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_[_]]`(Term.TypeLevel.Var.`UserDefinedType[_[_]]`(nme, None))

    @scala.annotation.targetName("trait(*->*)->*->*")
    def TRAIT[T[_[_], _], H[_], A](
      nme: String,
      tparamF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]],
      tparamA: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
      members: (`Type[_]`[H], `Type`[A]) => StateT[ErrorF, (Set[LibDep], List[Statement]), Unit]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[T]] =
      for
        f <- StateT.liftF(tparamF.runEmptyA)
        a <- StateT.liftF(tparamA.runEmptyA)
        (ds, ms) <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), (Set[LibDep], List[Statement])](members(f, a).runEmptyS)
        c <- StateT.pure(Trait.`(*->*)->*->*`[T](nme, ms))
        d <- StateT.pure(TraitDef.`trait[_[_], _]`(0, f.tpe, a.tpe, c))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.ext(d))
        _ <- ds.toList.traverse(l => StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(l)))
      yield `Type[_[_], _]`(Term.TypeLevel.Var.`UserDefinedType[_[_], _]`(nme, None))