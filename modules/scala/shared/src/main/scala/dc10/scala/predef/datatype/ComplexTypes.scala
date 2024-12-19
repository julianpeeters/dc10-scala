package dc10.scala.predef.datatype

import cats.Id
import cats.data.StateT
import dc10.scala.dsl.{*, given}
import dc10.scala.*
import scala.language.implicitConversions

trait ComplexTypes[F[_]]:
  def LIST: F[`Type.*->*`[List]]
  def List: F[`Value.*->*`[[A] =>> List[A] => List[A]]]
  def OPTION: F[`Type.*->*`[Option]]
  def Option: F[`Value.*->*`[[A] =>> A => Option[A]]]
  def SET: F[`Type.*->*`[Set]]
  def Set: F[`Value.*->*`[[A] =>> A => Set[A]]]
  def Some: F[`Value.*->*`[[A] =>> A => Option[A]]]
  def TUPLE: F[`Type.*->*->*`[Tuple2]]
  def Tuple[A, B]: (F[`Value.*`[A]], F[`Value.*`[B]]) => F[`Value.*`[Tuple2[A, B]]]
  def TUPLE3: F[`Type.*->*->*->*`[Tuple3]]
  def Tuple3[A, B, C]: (F[`Value.*`[A]], F[`Value.*`[B]], F[`Value.*`[C]]) => F[`Value.*`[Tuple3[A, B, C]]]

object ComplexTypes:

  trait Mixins extends ComplexTypes[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:
      
    def LIST: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[List]] =
      StateT.pure(Type.`Var[_]`[List](0, "List", None))

    def List: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> List[A] => List[A]]] =
      for
        t <- TYPE("A").==>>[[A] =>> List[A] => List[A]](a => LIST(a) ==> LIST(a))
      yield Value.`Var1[_]`(0, "List", t, None)

    def OPTION: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[Option]] =
      StateT.pure(Type.`Var[_]`(0, "Option", None))

    def Option: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> A => Option[A]]] =
      for
        t <- TYPE("A").==>>[[A] =>> A => Option[A]](a => (a ==> OPTION(a)))
      yield Value.`Var1[_]`[Id, Option](0, "Option", t, None)

    def SET: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[Set]] =
      StateT.pure(Type.`Var[_]`[Set](0, "Set", None))

    def Set: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> A => Set[A]]] =
      for
        t <- TYPE("A").==>>[[A] =>> A => Set[A]](a => (a ==> SET(a)))
      yield Value.`Var1[_]`[Id, Set](0, "Option", t, None)
   
    def Some: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*->*`[[A] =>> A => Option[A]]] =
      for
        t <- TYPE("A").==>>[[A]=>> Id[A] => Option[A]](a => a ==> OPTION(a))
      yield Value.`Var1[_]`[Id, Option](0, "Some", t, None)

    def TUPLE: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*->*`[Tuple2]] =
      StateT.pure(Type.`Var[_, _]`(0, "Tuple2", None))
      
    def Tuple[A, B]: (
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]],
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]]
    ) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Tuple2[A, B]]] =
      (arg1, arg2) =>
        for
          a <- arg1
          b <- arg2
          t <- TUPLE(a.tpe, b.tpe)
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Tuple2[A, B]]](
            Value.App2(
              0,
              Value.VarA(0, "", Type.`App[_, _, _]`(0, Type.`Var[_, _, _]`(0, "=>", None), a.tpe, b.tpe, t)),
              a,
              b,
              t
            )
          )
        yield v

    def TUPLE3: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*->*->*`[Tuple3]] =
      StateT.pure(Type.`Var[_, _, _]`(0, "Tuple3", None))

    def Tuple3[A, B, C]: (
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[A]],
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[B]],
      StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[C]]
    ) => StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Tuple3[A, B, C]]] =
      (arg1, arg2, arg3) =>
        for
          a <- arg1
          b <- arg2
          c <- arg3
          t <- TUPLE3(a.tpe, b.tpe, c.tpe)
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Tuple3[A, B, C]]](
            Value.App3(
              0,
              Value.VarA(0, "", Type.`App[_, _, _, _]`(0, Type.`Var[_, _, _, _]`(0, "=>", None), a.tpe, b.tpe, c.tpe, t)),
              a,
              b,
              c,
              t
            )
          )
        yield v