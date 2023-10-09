package dc10.scala.predef.datatype

import cats.data.StateT
import cats.Eval
import cats.free.Cofree
import cats.implicits.*
import dc10.scala.Statement
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.TypeLevel.__
import dc10.scala.Symbol.Term.ValueLevel.{AppCtor1, AppVargs}
import dc10.scala.Symbol.Term.ValueLevel.Var.{ListCtor, OptionCtor}
import dc10.scala.ctx.ErrorF


trait ComplexTypes[F[_]]:
  def LIST: F[TypeExpr[List[__]]]
  def List[A]: F[ValueExpr[List[A] => List[A]]]
  extension [A] (list: F[ValueExpr[List[A] => List[A]]])
    @scala.annotation.targetName("appVL")
    def apply(args: F[ValueExpr[A]]*): F[ValueExpr[List[A]]]
  def OPTION: F[TypeExpr[Option[__]]]
  def Option[A]: F[ValueExpr[Option[A] => Option[A]]]
  extension [A] (option: F[ValueExpr[Option[A] => Option[A]]])
    @scala.annotation.targetName("appVO")
    def apply(arg: F[ValueExpr[A]]): F[ValueExpr[Option[A]]]


object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def LIST: StateT[ErrorF, List[Statement], TypeExpr[List[__]]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.ListType(None)))))
      
    def List[A]: StateT[ErrorF, List[Statement], ValueExpr[List[A] => List[A]]] =
      StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.ListCtor(None)))))
    
    extension [A] (list: StateT[ErrorF, List[Statement], ValueExpr[List[A] => List[A]]])
      @scala.annotation.targetName("appVL")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[A]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A]]] =
        for
          l <- list
          a <- args.toList.sequence
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.AppVargs(None, l.value, a.map(arg => arg.value)*))))

    def OPTION: StateT[ErrorF, List[Statement], TypeExpr[Option[__]]] =
      StateT.pure(TypeExpr(Cofree((), Eval.now(Term.TypeLevel.Var.OptionType(None)))))
      
    def Option[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A] => Option[A]]] =
      StateT.pure(ValueExpr(Cofree((), Eval.now(Term.ValueLevel.Var.OptionCtor(None)))))
    
    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Option[A] => Option[A]]])
      @scala.annotation.targetName("appVO")
      def apply(arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.Type[Option[A]]](Cofree((), Eval.now(Term.TypeLevel.App1(None, Cofree((), Eval.now(Term.TypeLevel.Var.OptionType(None))), a.value.tail.value.tpe))))
        yield ValueExpr(Cofree((), Eval.now(Term.ValueLevel.AppCtor1(None, t, a.value))))

