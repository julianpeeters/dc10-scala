package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.*
import dc10.scala.internal.implement.findImpl

trait PrimitiveTypes[F[_]]:

  type __

  def __ : F[`Type.Var: *`[__]]

  def BOOLEAN: F[`Type.Var: *`[Boolean]]
  given bLit: Conversion[Boolean, F[`Value.Lit`[Boolean]]]
  
  def INT: F[`Type.Var: *`[Int]]
  given iLit: Conversion[Int, F[`Value.Lit`[Int]]]
  extension (fa: F[`Value.Lit`[Int]])
    def +:(fb: F[`Value.Lit`[Int]]): F[`Value.Lit`[Int]]
    
  def NOTHING[A]: F[`Type.Var: *`[A]]

  def STRING: F[`Type.Var: *`[String]]
  given sLit: Conversion[String, F[`Value.Lit`[String]]]

  def UNIT: F[`Type.Var: *`[Unit]]
  given uLit: Conversion[Unit, F[`Value.Lit`[Unit]]]
  
object PrimitiveTypes:

  def boolean: `Type.Var: *`[Boolean]              = `Type.Var: *`(0, "Boolean", None)
  def boolean(b: Boolean): `Value.Lit`[Boolean] = `Value.Lit.Boolean: *`(0, boolean, b) 
  def int: `Type.Var: *`[Int]                      = `Type.Var: *`(0, "Int", None)
  def int(i: Int): `Value.Lit`[Int]             = `Value.Lit.Int: *`(0, int, i)
  def nothing: `Type.Var: *`[Nothing]              = `Type.Var: *`(0, "Nothing", None)
  def string: `Type.Var: *`[String]                = `Type.Var: *`(0, "String", None)
  def string(s: String): `Value.Lit`[String]    = `Value.Lit.String: *`(0, string, s) 
  def underscore[A]: `Type.Var: *`[A]              = `Type.Var: *`(0, "_", None)
  def unit: `Type.Var: *`[Unit]                    = `Type.Var: *`(0, "Unit", None)
  def unit(u: Unit): `Value.Lit`[Unit]          = `Value.Lit.Unit: *`(0, unit, u)

  trait Mixins extends PrimitiveTypes[StateT[ErrorF, Γ, _]]:

    def __ : StateT[ErrorF, Γ, `Type.Var: *`[__]] =
      StateT.pure(underscore[__])

    def BOOLEAN: StateT[ErrorF, Γ, `Type.Var: *`[Boolean]] =
      StateT.pure(boolean)
      
    given bLit: Conversion[Boolean, StateT[ErrorF, Γ, `Value.Lit`[Boolean]]] =
      v => StateT.pure(boolean(v))

    def INT: StateT[ErrorF, Γ, `Type.Var: *`[Int]] =
      StateT.pure(int)

    given iLit: Conversion[Int, StateT[ErrorF, Γ, `Value.Lit`[Int]]] =
      v => StateT.pure(int(v))

    extension (fa: StateT[ErrorF, Γ, `Value.Lit`[Int]])
      def +:(fb: StateT[ErrorF, Γ, `Value.Lit`[Int]]): StateT[ErrorF, Γ, `Value.Lit`[Int]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- StateT.liftF(fb.runEmptyA)
          r <- StateT.liftF[ErrorF, Γ, `Value.Lit`[Int]]((a.findImpl, b.findImpl) match
            case (Some(`Value.Lit.Int: *`(in1, tpe1, i1)), Some(`Value.Lit.Int: *`(in2, tpe2, i2))) => Right(`Value.Lit.Int: *`(in1, tpe1, i1 + i2))
            case (_, _) => Left(List(Error("Not a concrete Int")))
          )
        yield r
        
    def NOTHING[A]: StateT[ErrorF, Γ, `Type.Var: *`[A]] =
      StateT.pure(nothing)

    def STRING: StateT[ErrorF, Γ, `Type.Var: *`[String]] =
      StateT.pure(string)
    
    given sLit: Conversion[String, StateT[ErrorF, Γ, `Value.Lit`[String]]] =
      v => StateT.pure(string(v))

    def UNIT: StateT[ErrorF, Γ, `Type.Var: *`[Unit]] =
      StateT.pure(unit)
    
    given uLit: Conversion[Unit, StateT[ErrorF, Γ, `Value.Lit`[Unit]]] =
      v => StateT.pure(unit(v))