package dc10.scala.predef.types

import dc10.scala.*
import dc10.scala.predef.types.primitive.Nothing
import dc10.scala.dsl.apply

object complex:
  def List: `Type.Var: x_x`[List] = `Type.Var: x_x`(0, AliasSym("List"), None)
  def Nil[A]: `Value.Lit.List: x_x x`[A] = 
    `Value.Lit.List: x_x x`[A](0, List(Nothing), scala.Nil)
  extension [A] (head: `Value: x`[A])
    def ::(tail: `Value.Lit.List: x_x x`[A]): `Value.Lit.List: x_x x`[A] =
      `Value.Lit.List: x_x x`[A](0, List(`Type.Var: x`[A](0, AliasSym("A"), None)), head +: tail.l)
  // given listLit[A]: Conversion[List[`Value: x`[A]], `Value.Lit.List: x_x x`[A]] = d => `Value.Lit.List: x_x x`(0, List(`Type.Var: x`[A](0, AliasSym("A"), None)), d)
  def Option: `Type.Var: x_x`[Option] = `Type.Var: x_x`(0, AliasSym("Option"), None)
  
