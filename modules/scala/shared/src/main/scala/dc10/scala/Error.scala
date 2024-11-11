package dc10.scala

type ErrorF[A] = Either[List[Error], A]

case class Error(msg: String)
