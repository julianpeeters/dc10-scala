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
      targ.map(a => TypeExpr(Term.TypeLevel.App.App1(Term.TypeLevel.Var.UserDefinedType("List", None), a.tpe)))
      
    def List[A](args: StateT[ErrorF, List[Statement], ValueExpr[A]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A]]] =
      for
        a <- args.toList.sequence
        v <- a.headOption.fold[StateT[ErrorF, List[Statement], ValueExpr[List[A]]]](
          LIST(StateT.pure[ErrorF, List[Statement], TypeExpr[A]](TypeExpr(Term.TypeLevel.Var.NothingType()))).map(l => 
            ValueExpr(Term.ValueLevel.App.AppVargs(
              Term.ValueLevel.Var.UserDefinedValue("List", l.tpe, None),
              l.tpe,
              Nil*
            ))
          )
        )(h => LIST(StateT.pure(TypeExpr(h.value.tpe))).map(l => 
          ValueExpr(Term.ValueLevel.App.AppVargs(
            Term.ValueLevel.Var.UserDefinedValue("List", l.tpe, None),
            l.tpe,
            a.map(arg => arg.value)*)
          ))
        )
      yield v

    def OPTION[A](targ: StateT[ErrorF, List[Statement], TypeExpr[A]]): StateT[ErrorF, List[Statement], TypeExpr[Option[A]]] =
      targ.map(a => TypeExpr(Term.TypeLevel.App.App1(Term.TypeLevel.Var.UserDefinedType("Option", None), a.tpe)))
    
    def Option[A](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      for
        a <- arg
        t <- OPTION(StateT.pure(TypeExpr(a.value.tpe)))
        v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Option", t.tpe, None),
            a.value,
            t.tpe,
          )
        )
      yield ValueExpr(v)

    def Some[A](arg: StateT[ErrorF, List[Statement], ValueExpr[A]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A]]] =
      for
        a <- arg
        t <- OPTION(StateT.pure(TypeExpr(a.value.tpe)))
        v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Some", t.tpe, None),
            a.value,
            t.tpe,
          )
        )
      yield ValueExpr(v)

    def TUPLE[A, B](arg1: StateT[ErrorF, List[Statement], TypeExpr[A]], arg2: StateT[ErrorF, List[Statement], TypeExpr[B]]): StateT[ErrorF, List[Statement], TypeExpr[Tuple2[A, B]]] =
      for 
        a <- arg1
        b <- arg2
      yield TypeExpr(Term.TypeLevel.App.App2(Term.TypeLevel.Var.UserDefinedType("Tuple2", None), a.tpe, b.tpe))
      
    def Tuple[A, B]: (StateT[ErrorF, List[Statement], ValueExpr[A]], StateT[ErrorF, List[Statement], ValueExpr[B]]) => StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B]]] =
      (arg1, arg2) =>
        for
          a <- arg1
          b <- arg2
          t <- TUPLE(StateT.pure(TypeExpr(a.value.tpe)), StateT.pure(TypeExpr(b.value.tpe)))
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Tuple2[A, B]]](
            Term.ValueLevel.App.App2(
              Term.ValueLevel.Var.UserDefinedValue("", Term.TypeLevel.App.App3(Term.TypeLevel.Lam.Function2Type(), a.value.tpe, b.value.tpe, t.tpe), None),
              a.value,
              b.value,
              t.tpe
            )
          )
        yield ValueExpr(v)