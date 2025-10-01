package dc10.scala

import dc10.{Compiler, Error}

type ErrorF[A] = Either[List[Error], A]
// case class Error(msg: String)

case class Dep(org: String, nme: String, ver: String)

given compiler: Compiler[Statement, Dep, Error] =
  Compiler.impl[Statement, Dep, Error]