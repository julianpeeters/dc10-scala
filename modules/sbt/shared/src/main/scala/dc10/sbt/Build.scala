package dc10.sbt

import dc10.scala.Statement

sealed trait Build
object Build:

  case class SourceDir(
    path: List[String],
    deps: Set[Statement],
    // files: List[SourceFile[NonEmptyList, Statement]]
  ) extends Build

