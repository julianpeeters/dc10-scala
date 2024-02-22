package dc10.scala.predef.datatype

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.Symbol.Term.ValueLevel.Var.{ListCtor, OptionCtor}

trait ComplexTypes[F[_]]:
  def LIST[A]: F[TypeExpr[List[A], Unit]]
  def List[A]: F[ValueExpr[List[A], Unit]]
  extension [A, Z] (list: F[ValueExpr[List[A], Z]])
    @scala.annotation.targetName("appVL")
    def apply[Y](args: F[ValueExpr[A, Y]]*): F[ValueExpr[List[A], (Z, Y)]]
  def OPTION[A]: F[TypeExpr[Option[A], Unit]]
  def Option[A]: F[ValueExpr[Option[A], Unit]]
  extension [A] (option: F[ValueExpr[Option[A], Unit]])
    @scala.annotation.targetName("appVO")
    def apply[Z](arg: F[ValueExpr[A, Z]]): F[ValueExpr[Option[A], (Unit, Z)]]
  extension [A] (option: F[ValueExpr[Some[A], Unit]])
    @scala.annotation.targetName("appVOS")
    def apply[Z](arg: F[ValueExpr[A, Z]]): F[ValueExpr[Some[A], (Unit, Z)]]
  def Some[A]: F[ValueExpr[Option[A], Unit]]
  def TUPLE[A, B]: F[TypeExpr[Tuple2[A, B], Unit]]
  def Tuple[A, B]: F[ValueExpr[Tuple2[A, B], Unit]]
  extension [A, B] (list: F[ValueExpr[Tuple2[A, B], Unit]])
    @scala.annotation.targetName("appVT")
    def apply[Y, Z](arg1: F[ValueExpr[A, Y]], arg2: F[ValueExpr[B, Z]]): F[ValueExpr[Tuple2[A, B], (Unit, (Y, Z))]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def LIST[A]: StateT[ErrorF, List[Statement], TypeExpr[List[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.ListType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    def List[A]: StateT[ErrorF, List[Statement], ValueExpr[List[A], Unit]] =
      StateT.pure[ErrorF, List[Statement], ValueExpr[List[A], Unit]](ValueExpr(Term.ValueLevel.Var.ListCtor(None, Term.TypeLevel.Var.ListType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))))
    
    extension [A, Z] (list: StateT[ErrorF, List[Statement], ValueExpr[List[A], Z]])
      @scala.annotation.targetName("appVL")
      def apply[Y](args: StateT[ErrorF, List[Statement], ValueExpr[A, Y]]*): StateT[ErrorF, List[Statement], ValueExpr[List[A], (Z, Y)]] =
        for
          l <- list
          a <- args.toList.sequence
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[List[A], (Z, Y)]](a.headOption.fold(
              ??? // TODO: make a NOTHING type for empty
            )(h =>
              Term.ValueLevel.App.AppVargs(
                None,
                l.value,
                Term.TypeLevel.Var.ListType(None, Term.ValueLevel.App.AppCtor2(None, "",
                      Term.TypeLevel.App.App2(
                        None,
                        Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                        l.value.tpe.dep.tpe,
                        h.value.tpe.dep.tpe,
                        h.value.tpe.dep.tpe.dep
                    ),
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                  )
                ),
                a.map(arg => arg.value)*)
            )
          )
        yield ValueExpr(v)

    def OPTION[A]: StateT[ErrorF, List[Statement], TypeExpr[Option[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.OptionType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    def Option[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.OptionCtor(None, Term.TypeLevel.Var.OptionType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))))
    
    extension [A] (option: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]])
      @scala.annotation.targetName("appVO")
      def apply[Z](arg: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]): StateT[ErrorF, List[Statement], ValueExpr[Option[A], (Unit, Z)]] =
        for
          o <- option
          a <- arg
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[Option[A], (Unit, Z)]](Term.TypeLevel.App.App1(
            None,
            Term.TypeLevel.Var.OptionType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
            a.value.tpe,
            Term.ValueLevel.App.AppCtor2(None, "",
                  Term.TypeLevel.App.App2(
                    None,
                    Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                    o.value.tpe.dep.tpe,
                    a.value.tpe.dep.tpe,
                    a.value.tpe.dep.tpe.dep
                ),
                Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
              )
            )
          )
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
            Term.ValueLevel.App.AppCtor2(None, "",
              Term.TypeLevel.App.App2(
                None,
                Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                o.value.tpe.dep.tpe,
                a.value.tpe.dep.tpe,
                a.value.tpe.dep.tpe.dep
            ),
            Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
          )
          ))
        yield ValueExpr(Term.ValueLevel.App.AppPure(None, Term.ValueLevel.Var.SomeCtor(None, Term.TypeLevel.Var.SomeType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))), a.value, t))
       
    def Some[A]: StateT[ErrorF, List[Statement], ValueExpr[Option[A], Unit]] =
      StateT.pure(ValueExpr(Term.ValueLevel.Var.SomeCtor(None, Term.TypeLevel.Var.SomeType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))))

    def TUPLE[A, B]: StateT[ErrorF, List[Statement], TypeExpr[Tuple2[A, B], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    def Tuple[A, B]: StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B], Unit]] =
      StateT.pure[ErrorF, List[Statement], ValueExpr[Tuple2[A, B], Unit]](ValueExpr(Term.ValueLevel.Var.TupleCtor(None, Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())))))

    extension [A, B] (tuple: StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B], Unit]])
      @scala.annotation.targetName("appVT")
      def apply[Y, Z](arg1: StateT[ErrorF, List[Statement], ValueExpr[A, Y]], arg2: StateT[ErrorF, List[Statement], ValueExpr[B, Z]]): StateT[ErrorF, List[Statement], ValueExpr[Tuple2[A, B], (Unit, (Y, Z))]] =
        for
          l <- tuple
          a <- arg1
          b <- arg2
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[Tuple2[A, B], (Unit, (Y, Z))]](Term.ValueLevel.App.AppCtor2(
            None,
            "", 
            Term.TypeLevel.Var.TupleType(
              None,
              Term.ValueLevel.App.AppCtor2(None, "",
                Term.TypeLevel.App.App2(
                  None,
                  Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                  Term.TypeLevel.Var.UnitType(None),
                  Term.TypeLevel.Var.TupleType(
                    None,
                    a.value.tpe.dep
                  ),  
                  a.value.tpe.dep.tpe.dep
                ),
                a.value.tpe.dep,
                a.value.tpe.dep.tpe.dep
              )
            ),
            a.value,
            b.value
          ))
        yield ValueExpr(v)