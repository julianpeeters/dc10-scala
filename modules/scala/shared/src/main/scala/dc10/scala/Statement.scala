package dc10.scala

import cats.data.NonEmptyList

sealed trait Statement
case class CaseDef[T](lambda: `Value: *`[T]) extends Statement
case class CaseClassDef[T](tpe: `Type.Var: *`[T], fields: List[Statement], body: List[Statement]) extends Statement
case class LibDep(org: String, nme: String, ver: String) extends Statement
case class ObjDef[T](obj: `Value.Obj: *`[T]) extends Statement
case class PackageDef(nme: List[String], contents: NonEmptyList[Statement]) extends Statement
case class `DefDef: *`[T](value: `Value.Def.0: *`[T]) extends Statement
case class `DefDef: *→* *`[T[_], A](value: `Value.Def.0: *→* *`[T, A]) extends Statement
case class `DefDef: *→*→* * *`[A, B](value: `Value.Def.1: *→*→* * *`[A, B]) extends Statement
case class `TypeDef: *`[T](tpe: `Type.Var: *`[T]) extends Statement
case class `TypeDef: *→* *`[F[_], A](tpe: `Type.Var: *→* *`[F, A]) extends Statement
case class `ValDef: *`[T](value: `Value.Val: *`[T]) extends Statement
case class `ValDef: *→* *`[T[_], A](value: `Value.Val: *→* *`[T, A]) extends Statement
case class `ValDef: *→*→* * *`[T[_, _], A, B](value: `Value.Val: *→*→* * *`[T, A, B]) extends Statement
case class `ValDef: *→*→* * ((*→*)→*→* *→* *)`[T[_, _], G[_[_], _], H[_], A, B](value: `Value.Val: *→*→* * ((*→*)→*→* *→* *)`[T, G, H, A, B]) extends Statement
case class `ValDef: (*→*)→*→* *→* *`[T[_[_], _], F[_], A](value: `Value.Val: (*→*)→*→* *→* *`[T, F, A]) extends Statement