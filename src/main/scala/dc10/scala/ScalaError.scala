package dc10.scala

type ErrorF[A] = Either[List[ScalaError], A]

sealed trait ScalaError
case class IdentifierStatementExpected(butFound: Statement) extends ScalaError
case class IdentifierSymbolExpected(butFound: Symbol) extends ScalaError
case class TooManyExtensionArguments() extends ScalaError

