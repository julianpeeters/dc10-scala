package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{ErrorF, Statement}
import dc10.scala.predef.{Functions, Variables}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term

trait ComplexTypes[F[_]]:
  def LIST[A](targ: F[TypeExpr[A]]): F[TypeExpr[List[A]]]
  def List[A](args: F[ValueExpr[A]]*): F[ValueExpr[List[A]]]
  def OPTION[A](targ: F[TypeExpr[A]]): F[TypeExpr[Option[A]]]
  def Option[A](arg: F[ValueExpr[A]]): F[ValueExpr[Option[A]]]
  def Some[A](arg: F[ValueExpr[A]]): F[ValueExpr[Option[A]]]
  def TUPLE[A, B](arg1: F[TypeExpr[A]], arg2: F[TypeExpr[B]]): F[TypeExpr[Tuple2[A, B]]]
  def Tuple[A, B]: (F[ValueExpr[A]], F[ValueExpr[B]]) => F[ValueExpr[Tuple2[A, B]]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]
    with Functions.Mixins with Variables.Mixins:

    def LIST[A](targ: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[List[A]]] =
      targ.map(a => TypeExpr(Term.TypeLevel.Var.ListType(a.tpe)))
      
    def List[A](args: StateT[ErrorF, List[Statement], ValueExpr[A]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A]]] =
      for
        a <- args.toList.sequence
        v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[List[A]]](a.headOption.fold(
            ??? // TODO: make a NOTHING type for empty
          )(h =>
            Term.ValueLevel.App.AppVargs(
              Term.ValueLevel.Var.UserDefinedValue("List", Term.TypeLevel.Var.ListType(h.value.tpe), None),
              Term.TypeLevel.Var.ListType(h.value.tpe),
              a.map(arg => arg.value)*)
          )
        )
      yield ValueExpr(v)

    def OPTION[A](targ: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[Option[A]]] =
      targ.map(a => TypeExpr(Term.TypeLevel.Var.OptionType(a.tpe)))
    
    def Option[A](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      for
        a <- arg
        v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Option", Term.TypeLevel.Var.OptionType(a.value.tpe), None),
            a.value,
            Term.TypeLevel.Var.OptionType(a.value.tpe),
          )
        )
      yield ValueExpr(v)

    def Some[A](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      for
        a <- arg
        v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Some", Term.TypeLevel.Var.SomeType(a.value.tpe), None),
            a.value,
            Term.TypeLevel.Var.SomeType(a.value.tpe),
          )
        )
      yield ValueExpr(v)

    def TUPLE[A, B](arg1: StateT[ErrorF, List[Statement], TypeExpr[A]], arg2: StateT[ErrorF, List[Statement], TypeExpr[B]]): StateT[ErrorF, List[Statement], TypeExpr[Tuple2[A, B]]] =
      for 
        a <- arg1
        b <- arg2
      yield TypeExpr(Term.TypeLevel.Var.TupleType(a.tpe, b.tpe))
      
    def Tuple[A, B]: (StateT[ErrorF, List[Statement], ValueExpr[A]], StateT[ErrorF, List[Statement], ValueExpr[B]]) => StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]] =
      (arg1, arg2) =>
        for
          a <- arg1
          b <- arg2
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[(A, B) => Tuple2[A, B]]](Term.TypeLevel.App.App3(Term.TypeLevel.Lam.Function2Type(), a.value.tpe, b.value.tpe, Term.TypeLevel.Var.TupleType(a.value.tpe, b.value.tpe)))
          c <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Tuple2[A, B]]](
            Term.ValueLevel.App.App2(
              Term.ValueLevel.Var.UserDefinedValue("", t, None),
              a.value,
              b.value,
              Term.TypeLevel.App.App2(Term.TypeLevel.Var.TupleType(a.value.tpe, b.value.tpe), a.value.tpe, b.value.tpe)
            )
          )
        yield ValueExpr(c)