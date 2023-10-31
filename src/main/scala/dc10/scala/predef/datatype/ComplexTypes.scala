package dc10.scala.predef.datatype

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.TypeLevel.__
import dc10.scala.Symbol.Term.ValueLevel.App.{AppPure, AppVargs}
import dc10.scala.Symbol.Term.ValueLevel.Var.{ListCtor, OptionCtor}

trait ComplexTypes[F[_]]:
  def LIST[G[_] <: List[?]]: F[TypeExpr[Unit, G[__]]]
  def List[A]: F[ValueExpr[Unit, List[A] => List[A]]]
  extension [A] (list: F[ValueExpr[Unit, List[A] => List[A]]])
    @scala.annotation.targetName("appVL")
    def apply(args: F[ValueExpr[Unit, A]]*): F[ValueExpr[Unit, List[A]]]
  def OPTION[G[_] <: Option[?]]: F[TypeExpr[Unit, G[__]]]
  def Option[A]: F[ValueExpr[Unit, A => Option[A]]]
  extension [A] (option: F[ValueExpr[Unit, A => Option[A]]])
    @scala.annotation.targetName("appVO")
    def apply(arg: F[ValueExpr[Unit, A]]): F[ValueExpr[Unit, Option[A]]]
  def Some[A]: F[ValueExpr[Unit, A => Some[A]]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def LIST[G[_] <: List[?]]: StateT[ErrorF, List[Statement], TypeExpr[Unit, G[__]]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.ListType(None)))))
      
    def List[A]: StateT[ErrorF, List[Statement], ValueExpr[Unit, List[A] => List[A]]] =
      StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.ListCtor(None)))))
    
    extension [A] (list: StateT[ErrorF, List[Statement], ValueExpr[Unit, List[A] => List[A]]])
      @scala.annotation.targetName("appVL")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]]*): StateT[ErrorF, List[Statement], ValueExpr[Unit, List[A]]] =
        for
          l <- list
          a <- args.toList.sequence
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.AppVargs(None, l.value, a.map(arg => arg.value)*))))

    def OPTION[G[_] <: Option[?]]: StateT[ErrorF, List[Statement], TypeExpr[Unit, G[__]]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.OptionType(None)))))
      
    def Option[A]: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => Option[A]]] =
      StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.OptionCtor(None)))))
    
    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => Option[A]]])
      @scala.annotation.targetName("appVO")
      def apply(arg: StateT[ErrorF, List[Statement], ValueExpr[Unit, A]]): StateT[ErrorF, List[Statement], ValueExpr[Unit, Option[A]]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.Type[Unit, Option[A]]](Cofree((), Eval.now(Term.TypeLevel.App1(None, Cofree((), Eval.now(Term.TypeLevel.Var.OptionType(None))), a.value.tail.value.tpe))))
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.App.AppPure(None, o.value, a.value, t))))      

    def Some[A]: StateT[ErrorF, List[Statement], ValueExpr[Unit, A => Some[A]]] =
      StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.OptionCtor.SomeCtor(None)))))
