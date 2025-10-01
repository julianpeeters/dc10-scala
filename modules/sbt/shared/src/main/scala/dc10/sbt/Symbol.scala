package dc10.sbt

sealed trait Symbol

case class RepoSym(nme: String) extends Symbol

sealed trait Extras extends Symbol
object Extras:
  case class Gitignore() extends Extras
  case class License() extends Extras
  case class Readme(text: String) extends Extras
