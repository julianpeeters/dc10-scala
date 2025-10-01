package dc10.sbt

import dc10.scala.Statement

sealed trait Project
object Project:

  case class AddSbtPlugin(
    libDep: Statement
  ) extends Project

  case class CrossProject(
    nme: String,
    src: Build.SourceDir,
  ) extends Project

  case class Root(
    nme: String,
    agg: List[SubProject],
    src: Build.SourceDir,
  ) extends Project

  case class SubProject(
    nme: String,
    src: Build.SourceDir,
  ) extends Project