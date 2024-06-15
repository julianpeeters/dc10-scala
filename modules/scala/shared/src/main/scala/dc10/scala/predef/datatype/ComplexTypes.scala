package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{ErrorF, LibDep, Statement}
import dc10.scala.predef.{Functions, Variables}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_, _]`}
import dc10.scala.Statement.ValueExpr.{`Value`}
import dc10.scala.Symbol.Term


import scala.language.implicitConversions
import dc10.scala.predef.Applications

trait ComplexTypes[F[_]]:
  def LIST: F[`Type[_]`[List]]
  def List[A](args: F[`Value`[A]]*): F[`Value`[List[A]]]
  def OPTION: F[`Type[_]`[Option]]
  def Option[A](arg: F[`Value`[A]]): F[`Value`[Option[A]]]
  def Some[A](arg: F[`Value`[A]]): F[`Value`[Option[A]]]
  def TUPLE: F[`Type[_, _]`[Tuple2]]
  def Tuple[A, B]: (F[`Value`[A]], F[`Value`[B]]) => F[`Value`[Tuple2[A, B]]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, (Set[LibDep], List[Statement]), A]]
    with Applications.Mixins with Functions.Mixins with PrimitiveTypes.Mixins with Variables.Mixins:
      
    def LIST: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[List]] =
      StateT.pure(`Type[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`("List", None)))
      
    def List[A](
      args: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]]*
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[List[A]]] =
      for
        a <- args.toList.sequence
        n <- NOTHING
        v <- a.headOption.fold[StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[List[A]]]](
          LIST(StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Type`[Nothing]](n)).map(l => 
            Value(Term.ValueLevel.App.AppVargs(
              Term.ValueLevel.Var.UserDefinedValue("List", l.tpe, None),
              l.tpe,
              Nil*
            ))
          )
        )(h => LIST(StateT.pure(`Type`(h.value.tpe))).map(l => 
          Value(Term.ValueLevel.App.AppVargs(
            Term.ValueLevel.Var.UserDefinedValue("List", l.tpe, None),
            l.tpe,
            a.map(arg => arg.value)*)
          ))
        )
      yield v

    def OPTION: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[Option]] =
      `Type[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`("Option", None))
    
    def Option[A](arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[Option[A]]] =
      for
        a <- arg
        t <- OPTION(StateT.pure(`Type`(a.value.tpe)))
        v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Option", t.tpe, None),
            a.value,
            t.tpe,
          )
        )
      yield Value(v)

    def Some[A](arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[Option[A]]] =
      for
        a <- arg
        t <- OPTION(StateT.pure(`Type`(a.value.tpe)))
        v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[Option[A]]](
          Term.ValueLevel.App.AppPure(
            Term.ValueLevel.Var.UserDefinedValue("Some", t.tpe, None),
            a.value,
            t.tpe,
          )
        )
      yield Value(v)

    def TUPLE: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_, _]`[Tuple2]] =
      StateT.pure(`Type[_, _]`(Term.TypeLevel.Var.`UserDefinedType[_, _]`("Tuple2", None)))
      
    def Tuple[A, B]: (
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[A]],
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[B]]
    ) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[Tuple2[A, B]]] =
      (arg1, arg2) =>
        for
          a <- arg1
          b <- arg2
          t <- TUPLE(StateT.pure(`Type`(a.value.tpe)), StateT.pure(`Type`(b.value.tpe)))
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[Tuple2[A, B]]](
            Term.ValueLevel.App.App2(
              Term.ValueLevel.Var.UserDefinedValue("", Term.TypeLevel.App.`App[_, _, _]`(Term.TypeLevel.Var.`UserDefinedType[_, _, _]`("=>", None), a.value.tpe, b.value.tpe, t.tpe), None),
              a.value,
              b.value,
              t.tpe
            )
          )
        yield Value(v)