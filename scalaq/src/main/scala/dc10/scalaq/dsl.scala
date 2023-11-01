package dc10.scalaq

import dc10.scala.predef.{Applications, Functions, Variables}
import dc10.scala.predef.datatype.{ComplexTypes, PrimitiveTypes, TemplateTypes}
import dc10.scala.predef.file.Files
import dc10.scala.predef.namespace.{Objects, Packages}

trait dsl

object dsl extends dsl
  // Lambda calculus
  with Applications.Mixins with Functions.Mixins with Variables.Mixins
  // Datatypes
  with ComplexTypes.Mixins with PrimitiveTypes.Mixins with TemplateTypes.Mixins
  // Namespaces
  with Objects.Mixins with Packages.Mixins
  // Source files
  with Files.Mixins
  // Dependent Vectors
  with Vectors.Mixins