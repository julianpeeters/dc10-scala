package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.{*}

trait PrimitiveTypes[F[_]]:

  type __
  def __ : F[Type.`Var`[__]]

  def BOOLEAN: F[`Type.*`[Boolean]]
  given bLit: Conversion[Boolean, F[`Value.*`[Boolean]]]
  
  def INT: F[`Type.*`[Int]]
  given iLit: Conversion[Int, F[`Value.*`[Int]]]
  extension (fa: F[`Value.*`[Int]])
    def +:(fb: F[`Value.*`[Int]]): F[`Value.*`[Int]]
    
  def NOTHING[A]: F[`Type.*`[A]]

  def STRING: F[`Type.*`[String]]
  given sLit: Conversion[String, F[`Value.*`[String]]]

  def UNIT: F[`Type.*`[Unit]]
  given uLit: Conversion[Unit, F[`Value.*`[Unit]]]
  
object PrimitiveTypes:

  trait Mixins extends PrimitiveTypes[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    type __ = Type.__

    def __ : StateT[ErrorF, (Set[LibDep], List[Statement]), Type.`Var`[__]] =
      StateT.pure(Type.`Var`(0, "_", None))

    def BOOLEAN: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[Boolean]] =
      StateT.pure(Type.`Var`(0, "Boolean", None))
      
    given bLit: Conversion[Boolean, StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Boolean]]] =
      v =>
        BOOLEAN.flatMap(t => StateT.pure(Value.LitBoolean(0, t, v)))

    def INT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[Int]] =
      StateT.pure(Type.`Var`(0, "Int", None))

    given iLit: Conversion[Int, StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Int]]] =
      v => INT.flatMap(t => StateT.pure(Value.LitInt(0, t, v)))

    extension (fa: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Int]])
      def +:(fb: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Int]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Int]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- StateT.liftF(fb.runEmptyA)
          r <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Int]]((a.findImpl, b.findImpl) match
            case (Some(Value.LitInt(in1, tpe1, i1)), Some(Value.LitInt(in2, tpe2, i2))) => Right(Value.LitInt(in1, tpe1, i1 + i2))
            case (_, _) => Left(List(Error("Not a concrete Int")))
          )
        yield r
        
    def NOTHING[A]: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]] =
      StateT.pure(Type.`Var`(0, "Nothing", None))

    def STRING: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[String]] =
      StateT.pure(Type.`Var`(0, "String", None))
    
    given sLit: Conversion[String, StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]]] =
      v => STRING.flatMap(t => StateT.pure(Value.LitString(0, t, v)))

    def UNIT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[Unit]] =
      StateT.pure(Type.`Var`(0, "Unit", None))
    
    given uLit: Conversion[Unit, StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[Unit]]] =
      v => UNIT.flatMap(t => StateT.pure(Value.LitUnit(0, t, v)))