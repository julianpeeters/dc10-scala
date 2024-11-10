package dc10.scala

import dc10.Compiler

given compiler: Compiler[Statement, LibDep, Error] =
  Compiler.impl[Statement, LibDep, Error]