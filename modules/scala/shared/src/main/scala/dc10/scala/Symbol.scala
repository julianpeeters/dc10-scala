package dc10.scala

sealed abstract class Symbol:
  def nme: String

case class AliasSym(nme: String) extends Symbol

sealed trait DefSym extends Symbol

case class `DefSym.0`(nme: String) extends DefSym

case class `DefSym.1`[A, R](nme: String, arg1: `Value.Val: *`[A]) extends DefSym
object DefSym:
  extension (sym: `DefSym.0`)
    def apply[A, R](a: `Value.Val: *`[A]): `DefSym.1`[A, R] =
      `DefSym.1`(sym.nme, a)

case class FileSym(nme: String) extends Symbol

case class LzySym(nme: String) extends Symbol

case class ObjSym(nme: String) extends Symbol

case class PkgSym(nme: String) extends Symbol

case class ValSym(nme: String) extends Symbol

case class VarSym(nme: String) extends Symbol