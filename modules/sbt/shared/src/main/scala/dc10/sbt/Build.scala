package dc10.sbt

import cats.data.NonEmptyList
import dc10.file.SourceFile
import dc10.scala.Statement
import fs2.io.file.Path

sealed trait Build
object Build:

  case class SourceDir(
    path: Path,
    deps: Set[Statement],
    files: List[SourceFile[NonEmptyList, Statement]]
  ) extends Build

