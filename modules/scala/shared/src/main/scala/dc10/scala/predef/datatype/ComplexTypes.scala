package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel.Var.{ListCtor, OptionCtor}

trait ComplexTypes[F[_]]:
  def LIST[A]: F[TypeExpr[List[A]]]
  def List[A]: F[ValueExpr[List[A]]]
  extension [A] (list: F[ValueExpr[List[A]]])
    @scala.annotation.targetName("appVL")
    def apply[Y](args: F[ValueExpr[A]]*): F[ValueExpr[List[A]]]
  def OPTION[A]: F[TypeExpr[Option[A]]]
  def Option[A]: F[ValueExpr[Option[A]]]
  extension [A] (option: F[ValueExpr[Option[A]]])
    @scala.annotation.targetName("appVO")
    def apply[Z](arg: F[ValueExpr[A]]): F[ValueExpr[Option[A]]]
  extension [A] (option: F[ValueExpr[Some[A]]])
    @scala.annotation.targetName("appVOS")
    def apply[Z](arg: F[ValueExpr[A]]): F[ValueExpr[Some[A]]]
  def Some[A]: F[ValueExpr[Option[A]]]
  def TUPLE[A, B]: F[TypeExpr[Tuple2[A, B]]]
  def Tuple[A, B]: F[ValueExpr[Tuple2[A, B]]]
  extension [A, B] (list: F[ValueExpr[Tuple2[A, B]]])
    @scala.annotation.targetName("appVT")
    def apply[Y](arg1: F[ValueExpr[A]], arg2: F[ValueExpr[B]]): F[ValueExpr[Tuple2[A, B]]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def LIST[A]: StateT[ErrorF, List[Statement], TypeExpr[List[A]]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.ListType()))
      
    def List[A]: StateT[ErrorF, List[Statement], ValueExpr[List[A]]] =
      StateT.pure[ErrorF, List[Statement], ValueExpr[List[A]]](ValueExpr(Term.ValueLevel.Var.ListCtor(Term.TypeLevel.Var.ListType())))
    
    extension [A] (list: StateT[ErrorF, List[Statement], ValueExpr[List[A]]])
      @scala.annotation.targetName("appVL")
      def apply[Y](args: StateT[ErrorF, List[Statement], ValueExpr[A]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A]]] =
        for
          l <- list
          a <- args.toList.sequence
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[List[A]]](a.headOption.fold(
              ??? // TODO: make a NOTHING type for empty
            )(h =>
              Term.ValueLevel.App.AppVargs(
                l.value,
                Term.TypeLevel.Var.ListType(),
                a.map(arg => arg.value)*)
            )
          )
        yield ValueExpr(v)

    def OPTION[A]: StateT[ErrorF, List[Statement], TypeExpr[Option[A]]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.OptionType()))
      
    def Option[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.OptionCtor(Term.TypeLevel.Var.OptionType())))
    
    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Option[A]]])
      @scala.annotation.targetName("appVO")
      def apply[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[A]]](Term.TypeLevel.App.App1(Term.TypeLevel.Var.OptionType(), a.value.tpe))
        yield ValueExpr(Term.ValueLevel.App.AppPure(o.value, a.value, t))    

    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Some[A]]])
      @scala.annotation.targetName("appVOS")
      def apply[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Some[A]]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Some[A]]](Term.TypeLevel.App.App1(o.value.tpe, a.value.tpe))
        yield ValueExpr(Term.ValueLevel.App.AppPure(Term.ValueLevel.Var.SomeCtor(Term.TypeLevel.Var.SomeType()), a.value, t))
       
    def Some[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.SomeCtor(Term.TypeLevel.Var.SomeType())))

    def TUPLE[A, B]: StateT[ErrorF, List[Statement], TypeExpr[Tuple2[A, B]]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.TupleType()))
      
    def Tuple[A, B]: StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]] =
      StateT.pure[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]](ValueExpr(Term.ValueLevel.Var.TupleCtor(Term.TypeLevel.Var.TupleType())))

    extension [A, B] (tuple: StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]])
      @scala.annotation.targetName("appVT")
      def apply[Y](arg1: StateT[ErrorF, List[Statement], ValueExpr[A]], arg2: StateT[ErrorF, List[Statement], ValueExpr[B]]): StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]] =
        for
          l <- tuple
          a <- arg1
          b <- arg2
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Tuple2[A, B]]](Term.ValueLevel.App.AppCtor2(
            "", 
            Term.TypeLevel.Var.TupleType(),
            a.value,
            b.value
          ))
        yield ValueExpr(v)