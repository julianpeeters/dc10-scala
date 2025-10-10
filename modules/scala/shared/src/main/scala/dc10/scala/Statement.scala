package dc10.scala

import cats.data.NonEmptyList

sealed trait Statement
case class CaseDef[T](lambda: `Value: x`[T]) extends Statement
case class CaseClassDef[T](tpe: `Type.Var: x`[T], fields: List[Statement], body: List[Statement]) extends Statement
case class LibDep(org: String, nme: String, ver: String) extends Statement
case class ObjDef[T](obj: `Value.Obj: x`[T]) extends Statement
case class PackageDef(nme: List[String], contents: NonEmptyList[Statement]) extends Statement
case class `DefDef: x`[T](value: `Value.Def.0: x`[T]) extends Statement
case class `DefDef: x→x x`[T[_], A](value: `Value.Def.0: x→x x`[T, A]) extends Statement
case class `DefDef: x→x→x x x`[A, B](value: `Value.Def.1: x→x→x x x`[A, B]) extends Statement
case class `TypeDef: x`[T](tpe: `Type.Var: x`[T]) extends Statement
case class `TypeDef: x→x x`[F[_], A](tpe: `Type.Var: x→x x`[F, A]) extends Statement
case class `ValDef: x`[T](value: `Value.Val: x`[T]) extends Statement
case class `ValDef: x→x x`[T[_], A](value: `Value.Val: x→x x`[T, A]) extends Statement
case class `ValDef: x→x ((x→x)→x→x x→x x)`[T[_], F[_[_], _], G[_], A](value: `Value.Val: x→x ((x→x)→x→x x→x x)`[T, F, G, A]) extends Statement
case class `ValDef: x→x→x x x`[T[_, _], A, B](value: `Value.Val: x→x→x x x`[T, A, B]) extends Statement
case class `ValDef: x→x→x x ((x→x)→x→x x→x x)`[T[_, _], G[_[_], _], H[_], A, B](value: `Value.Val: x→x→x x ((x→x)→x→x x→x x)`[T, G, H, A, B]) extends Statement
case class `ValDef: x→x→x x (x→x ((x→x)→x→x x→x x))`[T[_, _], G[_[_], _], H[_], I[_], A, B](value: `Value.Val: x→x→x x (x→x ((x→x)→x→x x→x x))`[T, G, H, I, A, B]) extends Statement
case class `ValDef: (x→x)→x→x x→x x`[T[_[_], _], F[_], A](value: `Value.Val: (x→x)→x→x x→x x`[T, F, A]) extends Statement
case class `ValDef: (x→x)→x→x x→x ((x→x)→x x→x)`[T[_[_], _], F[_], G[_[_]], H[_]](value: `Value.Val: (x→x)→x→x x→x ((x→x)→x x→x)`[T, F, G, H]) extends Statement
