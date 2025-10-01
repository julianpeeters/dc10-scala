package dc10.scala.predef.types

import dc10.scala.*

object complex:

  def OPTION: `Type.Var: *→*`[Option] = `Type.Var: *→*`(0, AliasSym("Option"), None, () => ???)
  
