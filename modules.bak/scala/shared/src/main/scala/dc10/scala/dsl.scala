package dc10.scala

import dc10.scala.predef.binding.{Assignments, Matches, References}
import dc10.scala.predef.calculus.{Applications, Functions}
import dc10.scala.predef.calculus.variables.{Def, Type, Val}
import dc10.scala.predef.datatype.{ComplexTypes, PrimitiveTypes, TemplateTypes}
import dc10.scala.predef.file.Files
import dc10.scala.predef.namespace.{Objects, Packages}

trait dsl

object dsl extends dsl
  // Applications
  with Applications.Mixins
  // Functions
  with Functions.Mixins
  // Variables
  with Def.Mixins with Type.Mixins with Val.Mixins
  // Binding
  with Assignments.Mixins with Matches.Mixins with References.Mixins
  // Datatypes
  with ComplexTypes.Mixins with PrimitiveTypes.Mixins with TemplateTypes.Mixins
  // Namespaces
  with Objects.Mixins with Packages.Mixins
  // Source files
  with Files.Mixins