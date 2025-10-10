package dc10.scala.predef.datatype

import cats.data.StateT
import dc10.scala.*
import dc10.scala.internal.implement.findImpl

trait PrimitiveTypes[F[_]]:

  type __

  def __ : F[`Type.Var: x`[__]]

  def BOOLEAN: F[`Type.Var: x`[Boolean]]
  given bLit: Conversion[Boolean, F[`Value.Lit`[Boolean]]]
  
  def INT: F[`Type.Var: x`[Int]]
  given iLit: Conversion[Int, F[`Value.Lit`[Int]]]
  extension (fa: F[`Value.Lit`[Int]])
    def +:(fb: F[`Value.Lit`[Int]]): F[`Value.Lit`[Int]]
    
  def NOTHING[A]: F[`Type.Var: x`[A]]

  def STRING: F[`Type.Var: x`[String]]
  given sLit: Conversion[String, F[`Value.Lit`[String]]]

  def UNIT: F[`Type.Var: x`[Unit]]
  given uLit: Conversion[Unit, F[`Value.Lit`[Unit]]]
  
object PrimitiveTypes:

  def boolean: `Type.Var: x`[Boolean]              = `Type.Var: x`(0, "Boolean", None)
  def boolean(b: Boolean): `Value.Lit`[Boolean] = `Value.Lit.Boolean: x`(0, boolean, b) 
  def int: `Type.Var: x`[Int]                      = `Type.Var: x`(0, "Int", None)
  def int(i: Int): `Value.Lit`[Int]             = `Value.Lit.Int: x`(0, int, i)
  def nothing: `Type.Var: x`[Nothing]              = `Type.Var: x`(0, "Nothing", None)
  def string: `Type.Var: x`[String]                = `Type.Var: x`(0, "String", None)
  def string(s: String): `Value.Lit`[String]    = `Value.Lit.String: x`(0, string, s) 
  def underscore[A]: `Type.Var: x`[A]              = `Type.Var: x`(0, "_", None)
  def unit: `Type.Var: x`[Unit]                    = `Type.Var: x`(0, "Unit", None)
  def unit(u: Unit): `Value.Lit`[Unit]          = `Value.Lit.Unit: x`(0, unit, u)

  trait Mixins extends PrimitiveTypes[StateT[ErrorF, Γ, _]]:

    def __ : StateT[ErrorF, Γ, `Type.Var: x`[__]] =
      StateT.pure(underscore[__])

    def BOOLEAN: StateT[ErrorF, Γ, `Type.Var: x`[Boolean]] =
      StateT.pure(boolean)
      
    given bLit: Conversion[Boolean, StateT[ErrorF, Γ, `Value.Lit`[Boolean]]] =
      v => StateT.pure(boolean(v))

    def INT: StateT[ErrorF, Γ, `Type.Var: x`[Int]] =
      StateT.pure(int)

    given iLit: Conversion[Int, StateT[ErrorF, Γ, `Value.Lit`[Int]]] =
      v => StateT.pure(int(v))

    extension (fa: StateT[ErrorF, Γ, `Value.Lit`[Int]])
      def +:(fb: StateT[ErrorF, Γ, `Value.Lit`[Int]]): StateT[ErrorF, Γ, `Value.Lit`[Int]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- StateT.liftF(fb.runEmptyA)
          r <- StateT.liftF[ErrorF, Γ, `Value.Lit`[Int]]((a.findImpl, b.findImpl) match
            case (Some(`Value.Lit.Int: x`(in1, tpe1, i1)), Some(`Value.Lit.Int: x`(in2, tpe2, i2))) => Right(`Value.Lit.Int: x`(in1, tpe1, i1 + i2))
            case (_, _) => Left(List(Error("Not a concrete Int")))
          )
        yield r
        
    def NOTHING[A]: StateT[ErrorF, Γ, `Type.Var: x`[A]] =
      StateT.pure(nothing)

    def STRING: StateT[ErrorF, Γ, `Type.Var: x`[String]] =
      StateT.pure(string)
    
    given sLit: Conversion[String, StateT[ErrorF, Γ, `Value.Lit`[String]]] =
      v => StateT.pure(string(v))

    def UNIT: StateT[ErrorF, Γ, `Type.Var: x`[Unit]] =
      StateT.pure(unit)
    
    given uLit: Conversion[Unit, StateT[ErrorF, Γ, `Value.Lit`[Unit]]] =
      v => StateT.pure(unit(v))