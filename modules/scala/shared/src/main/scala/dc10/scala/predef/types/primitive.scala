package dc10.scala.predef.types

import dc10.scala.*

object primitive:

  def Boolean: `Type.Lit: x`[Boolean]                      = `Type.Lit: x`(0, "Boolean")
  given bLit: Conversion[Boolean, `Value.Lit.Boolean: x`]  = b => `Value.Lit.Boolean: x`(0, Boolean, b)

  def Int: `Type.Lit: x`[Int]                              = `Type.Lit: x`(0, "Int")
  given iLit: Conversion[Int, `Value.Lit.Int: x`]          = i => `Value.Lit.Int: x`(0, Int, i)

  def Long: `Type.Lit: x`[Long]                            = `Type.Lit: x`(0, "Long")
  given lLit: Conversion[Long, `Value.Lit.Long: x`]        = l => `Value.Lit.Long: x`(0, Long, l)

  def Float: `Type.Lit: x`[Float]                          = `Type.Lit: x`(0, "Float")
  given fLit: Conversion[Float, `Value.Lit.Float: x`]      = f => `Value.Lit.Float: x`(0, Float, f)

  def Double: `Type.Lit: x`[Double]                        = `Type.Lit: x`(0, "Double")
  given dLit: Conversion[Double, `Value.Lit.Double: x`]    = d => `Value.Lit.Double: x`(0, Double, d)

  def Nothing: `Type.Lit: x`[Nothing]                      = `Type.Lit: x`(0, "Nothing")

  def String: `Type.Lit: x`[String]                        = `Type.Lit: x`(0, "String")
  given sLit: Conversion[String, `Value.Lit.String: x`]    = s => `Value.Lit.String: x`(0, String, s)

  type __ = `Type.Lit: x`[?]
  def `__`: `Type.Lit: x`[__]                              = `Type.Lit: x`(0, "_")

  def Unit: `Type.Lit: x`[Unit]                            = `Type.Lit: x`(0, "Unit")
  given uLit: Conversion[Unit, `Value.Lit.Unit: x`]         = u => `Value.Lit.Unit: x`(0, Unit, u)