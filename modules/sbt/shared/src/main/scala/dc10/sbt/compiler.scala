package dc10.sbt

import cats.data.NonEmptyList
import dc10.{Compiler, given}
// import dc10.scala.SbtStatement

given compiler: Compiler[NonEmptyList, SbtStatement] =
  Compiler.impl[NonEmptyList, SbtStatement]