package dc10.scala

import dc10.Compiler

type ErrorF[A] = Either[List[Error], A]
case class Error(msg: String)

case class LibDep(org: String, nme: String, ver: String)

given compiler: Compiler[Statement, LibDep, Error] =
  Compiler.impl[Statement, LibDep, Error]