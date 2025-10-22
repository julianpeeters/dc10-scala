package dc10.scala

sealed abstract class Symbol:
  def nme: String

case class AliasSym(nme: String) extends Symbol

sealed trait DefSym extends Symbol

case class `DefSym.0`(nme: String) extends DefSym

case class `DefSym.1`[A, R](nme: String, arg1: `Value.Val: x`[A]) extends DefSym
case class `DefSym.1: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[G[_[_], _], H[_], A](nme: String, arg1: `Value.Val: lx_xl_x_x x_x x`[G, H, A]) extends DefSym
case class `DefSym.1_: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F[_,_], G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_]], L[_]](nme: String, arg1: `Value.Val: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x x_xll`[F, G, H, A, I, J, K, L]) extends DefSym
// case class `DefSym.1: x_x_x lx_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xlll llx_xl_x_x x_x llx_xl_x_x x_x xll`[G[_[_], _], H[_], A, I[_[_], _], J[_], K[_[_], _], L[_], B](nme: String, arg1: `Value.Val: x_x_x llx_xl_x_x x_x xl llx_xl_x_x x_x llx_xl_x_x x_x xll`[G, H, A, I, J, K, L, B]) extends DefSym
object DefSym:
  extension (sym: `DefSym.0`)
    def apply[A, R](a: `Value.Val: x`[A]): `DefSym.1`[A, R] =
      `DefSym.1`(sym.nme, a)
case class FileSym(nme: String) extends Symbol
case class LzySym(nme: String) extends Symbol
case class ObjSym(nme: String) extends Symbol
case class PkgSym(nme: String) extends Symbol
case class SldTrtSym(nme: String) extends Symbol
case class TrtSym(nme: String) extends Symbol
case class ValSym(nme: String) extends Symbol
case class VarSym(nme: String) extends Symbol