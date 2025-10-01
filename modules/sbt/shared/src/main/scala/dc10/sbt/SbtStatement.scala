package dc10.sbt

import dc10.sbt.Extras.{Gitignore, License, Readme}
import dc10.scala.{LibDep, Statement}

sealed trait SbtStatement
case class ProjectDef(project: Project) extends SbtStatement
case class LibDepStatement(statement: Statement) extends SbtStatement
case class LicenseStatement(license: License) extends SbtStatement
case class GitignoreStatement(gitignore: Gitignore) extends SbtStatement
case class ReadmeStatement(readme: Readme) extends SbtStatement
case class ScalaStatement(statement: Statement) extends SbtStatement

object SbtStatement:

  extension (statement: Statement)
    def asSbtStatement: SbtStatement =
      statement match
        case s@LibDep(org, nme, ver) => LibDepStatement(s)
        case s => ScalaStatement(s)
      
      
