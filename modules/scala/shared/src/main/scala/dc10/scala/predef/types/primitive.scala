package dc10.scala.predef.types

import dc10.scala.*

object primitive:

  def Boolean: `Type.Lit: *`[Boolean]                      = `Type.Lit: *`(0, "Boolean")
  given bLit: Conversion[Boolean, `Value.Lit.Boolean: *`]  = b => `Value.Lit.Boolean: *`(0, Boolean, b)

  def Int: `Type.Lit: *`[Int]                              = `Type.Lit: *`(0, "Int")
  given iLit: Conversion[Int, `Value.Lit.Int: *`]          = i => `Value.Lit.Int: *`(0, Int, i)

  def Long: `Type.Lit: *`[Long]                            = `Type.Lit: *`(0, "Long")
  given lLit: Conversion[Long, `Value.Lit.Long: *`]        = l => `Value.Lit.Long: *`(0, Long, l)

  def Float: `Type.Lit: *`[Float]                          = `Type.Lit: *`(0, "Float")
  given fLit: Conversion[Float, `Value.Lit.Float: *`]      = f => `Value.Lit.Float: *`(0, Float, f)

  def Double: `Type.Lit: *`[Double]                        = `Type.Lit: *`(0, "Double")
  given dLit: Conversion[Double, `Value.Lit.Double: *`]    = d => `Value.Lit.Double: *`(0, Double, d)

  def Nothing: `Type.Lit: *`[Nothing]                      = `Type.Lit: *`(0, "Nothing")

  def String: `Type.Lit: *`[String]                        = `Type.Lit: *`(0, "String")
  given sLit: Conversion[String, `Value.Lit.String: *`]    = s => `Value.Lit.String: *`(0, String, s)

  type __ = `Type.Lit: *`[?]
  def `__`: `Type.Lit: *`[__]                              = `Type.Lit: *`(0, "_")

  def Unit: `Type.Lit: *`[Unit]                            = `Type.Lit: *`(0, "Unit")
  given uLit: Conversion[Unit, `Value.Lit.Unit: *`]         = u => `Value.Lit.Unit: *`(0, Unit, u)