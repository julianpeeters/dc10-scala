package dc10.scala

import cats.data.NonEmptyList
import dc10.{Compiler, Error, given}

type ErrorF[A] = Either[List[Error], A]

given compiler: Compiler[NonEmptyList, Statement] =
  Compiler.impl[NonEmptyList, Statement]