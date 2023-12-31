package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel.App.{AppPure, AppVargs}
import dc10.scala.Symbol.Term.ValueLevel.Var.{ListCtor, OptionCtor}

trait ComplexTypes[F[_]]:
  def LIST[A]: F[TypeExpr[List[A], Unit]]
  def List[A]: F[ValueExpr[List[A], Unit]]
  extension [A] (list: F[ValueExpr[List[A], Unit]])
    @scala.annotation.targetName("appVL")
    def apply[Z](args: F[ValueExpr[A, Z]]*): F[ValueExpr[List[A], (Unit, Z)]]
  def OPTION[A]: F[TypeExpr[Option[A], Unit]]
  def Option[A]: F[ValueExpr[Option[A], Unit]]
  extension [A] (option: F[ValueExpr[Option[A], Unit]])
    @scala.annotation.targetName("appVO")
    def apply[Z](arg: F[ValueExpr[A, Z]]): F[ValueExpr[Option[A], (Unit, Z)]]
  extension [A] (option: F[ValueExpr[Some[A], Unit]])
    @scala.annotation.targetName("appVOS")
    def apply[Z](arg: F[ValueExpr[A, Z]]): F[ValueExpr[Some[A], (Unit, Z)]]
  def Some[A]: F[ValueExpr[Option[A], Unit]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def LIST[A]: StateT[ErrorF, List[Statement], TypeExpr[List[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.ListType(None, ())))
      
    def List[A]: StateT[ErrorF, List[Statement], ValueExpr[List[A], Unit]] =
      StateT.pure[ErrorF, List[Statement], ValueExpr[List[A], Unit]](ValueExpr(Term.ValueLevel.Var.ListCtor(None, Term.TypeLevel.Var.ListType(None, ()))))
    
    extension [A] (list: StateT[ErrorF, List[Statement], ValueExpr[List[A], Unit]])
      @scala.annotation.targetName("appVL")
      def apply[Z](args: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A], (Unit, Z)]] =
        for
          l <- list
          a <- args.toList.sequence
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[List[A], (Unit, Z)]](a.headOption.fold(
              ??? // TODO: make a NOTHING type for empty
            )(h =>
              Term.ValueLevel.App.AppVargs(
                None,
                l.value,
                Term.TypeLevel.Var.ListType(None, ((), h.value.tpe.dep)),
                a.map(arg => arg.value)*)
            )
          )
        yield ValueExpr(v)

    def OPTION[A]: StateT[ErrorF, List[Statement], TypeExpr[Option[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.OptionType(None, ())))
      
    def Option[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.OptionCtor(None, Term.TypeLevel.Var.OptionType(None, ()))))
    
    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]])
      @scala.annotation.targetName("appVO")
      def apply[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A], (Unit, Z)]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[A], (Unit, Z)]](Term.TypeLevel.App.App1(
            None,
            Term.TypeLevel.Var.OptionType(None, ()),
            a.value.tpe,
            ((), a.value.tpe.dep)
          ))
        yield ValueExpr(Term.ValueLevel.App.AppPure(None, o.value, a.value, t))    

    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Some[A], Unit]])
      @scala.annotation.targetName("appVOS")
      def apply[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]): StateT[ErrorF, List[Statement], ValueExpr[Some[A], (Unit, Z)]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Some[A], (Unit, Z)]](Term.TypeLevel.App.App1(
            None,
            o.value.tpe,
            a.value.tpe,
            ((), a.value.tpe.dep)
          ))
        yield ValueExpr(Term.ValueLevel.App.AppPure(None, Term.ValueLevel.Var.SomeCtor(None, Term.TypeLevel.Var.SomeType(None, ())), a.value, t))
       
    def Some[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.SomeCtor(None, Term.TypeLevel.Var.SomeType(None, ()))))
