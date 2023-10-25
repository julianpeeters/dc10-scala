package dc10.scala

type ErrorF[A] = Either[List[Error], A]

sealed trait Error
case class IdentifierStatementExpected(butFound: Statement) extends Error
case class IdentifierSymbolExpected(butFound: Symbol) extends Error
case class TooManyExtensionArguments() extends Error

