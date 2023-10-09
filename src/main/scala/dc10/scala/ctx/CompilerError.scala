package dc10.scala.ctx

import dc10.scala.{Statement, Symbol}

type ErrorF[A] = Either[List[CompilerError], A]

sealed trait CompilerError
case class IdentifierStatementExpected(butFound: Statement) extends CompilerError
case class IdentifierSymbolExpected(butFound: Symbol) extends CompilerError
