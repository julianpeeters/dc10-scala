package dc10.scala.predef.calculus

import dc10.scala.*
// trait function[F[_]]:

  // // Types
  // extension [A] (domain: `Type.Expr: *`[A])
  //   def ==>[B](codomain: `Type.Expr: *`[B]): `Type.AppInfix[_, _]`[Function1, A, B]

  // extension [A, B] (domain: (`Type.Expr: *`[A], `Type.Expr: *`[B]))
  //   def ==>[C](codomain: `Type.Expr: *`[C]): `Type.AppInfix[_, _, _]`[Function2, A, B, C]

  // extension [A, B, C] (domain: (`Type.Expr: *`[A], `Type.Expr: *`[B], `Type.Expr: *`[C]))
  //   def ==>[D](codomain: `Type.Expr: *`[D]): `Type.AppInfix[_, _, _, _]`[Function3, A, B, C, D]

  // // Values
  // extension [A] (domain: F[`Value.Var.Unbound.Data`[A]])
  //   def ==>[B](codomain: `Value.Var.Unbound.Data`[A] => `Value.Expr: *`[B]): F[`Value.Lam.1: *→*→* * *`[A, B]]

  // extension [A, B] (domain: (F[`Value.Var.Unbound.Data`[A]], F[`Value.Var.Unbound.Data`[B]]))
  //   def ==>[C](codomain: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => `Value.Expr: *`[C]): F[`Value.Lam.2: *→*→*→* * * *`[A, B, C]]

  // extension [A, B, C] (domain: (F[`Value.Var.Unbound.Data`[A]], F[`Value.Var.Unbound.Data`[B]], F[`Value.Var.Unbound.Data`[C]]))
  //   def ==>[D](codomain: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => `Value.Expr: *`[D]): F[`Value.Lam.3: *→*→*→*→* * * * *`[A, B, C, D]]

object function:

  // Types
  // def FUNCTION1[A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *`[B]): `Type.AppInfix: *→*→* * *`[Function1, A, B] =
  //   `Type.AppInfix: *→*→* * *`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *`[B]): `Type.AppInfix: *→*→* * *`[Function1, A, B] =
    `Type.AppInfix: *→*→* * *`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[F[_], A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *→* *`[F, B]): `Type.AppInfix: *→*→* * (*→* *)`[Function1, F, A, B] =
    `Type.AppInfix: *→*→* * (*→* *)`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[H[_], I[_[_], _], A, B](a: `Type.Expr: *`[A], b: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]): `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`[Function1, I, H, A, B] =
    `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[G[_[_], _], H[_], I[_], A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *→* ((*→*)→*→* *→* *)`[I, G, H, B]): `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, I, A, B] =
    `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[G[_], H[_], I[_[_], _], A, B](a: `Type.Expr: *→* *`[G, A], b: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]): `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B] =
    `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  // def FUNCTION1[F[_], A, B](a: `Type.Expr: *`[A], b: `Type.Expr: *→* *`[F, B]): `Type.AppInfix[A, F[B]]`[Function1, F, A, B] =
  //   `Type.AppInfix[A, F[B]]`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)  

  // def FUNCTION1[T[_[_], _], F[_], A, B](a: `Type.Expr: *→* *`[F, A], b: `Type.Expr: (*→*)→*→* *→* *`[T, F, B]): `Type.AppInfix: *→*→* * *`[Function1, F[A], B] =
  //   `Type.AppInfix[F[A], G[_[_], _]]`(0, `Type.Var: *→*→*`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION2[A, B, C](a: `Type: *`[A], b: `Type: *`[B], c: `Type: *`[C]): `Type.AppInfix: *→*→*→* * * *`[Function2, A, B, C] =
    `Type.AppInfix: *→*→*→* * * *`(0, `Type.Var: *→*→*→*`(0, AliasSym("=>"), scala.None), a, b, c)

  def FUNCTION3[A, B, C, D](a: `Type: *`[A], b: `Type: *`[B], c: `Type: *`[C], d: `Type: *`[D]): `Type.AppInfix: *→*→*→*→* * * * *`[Function3, A, B, C, D] =
    `Type.AppInfix: *→*→*→*→* * * * *`(0, `Type.Var: *→*→*→*→*`(0, AliasSym("=>"), scala.None), a, b, c, d)
  
  // Values
  def function1[A, B](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B]): `Value.Lam.1: *→*→* * *`[A, B] =
    `Value.Lam.1: *→*→* * *`(0, a, b, FUNCTION1(a.tpe, b.tpe))

  def function2[A, B, C](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B], c: `Value.Expr: *`[C]): `Value.Lam.2: *→*→*→* * * *`[A, B, C] =
    `Value.Lam.2: *→*→*→* * * *`(0, a, b, c, FUNCTION2(a.tpe, b.tpe, c.tpe))

  def function3[A, B, C, D](a: `Value.Expr: *`[A], b: `Value.Expr: *`[B], c: `Value.Expr: *`[C], d: `Value.Expr: *`[D]): `Value.Lam.3: *→*→*→*→* * * * *`[A, B, C, D] =
    `Value.Lam.3: *→*→*→*→* * * * *`(0, a, b, c, d, FUNCTION3(a.tpe, b.tpe, c.tpe, d.tpe))

  // Impl
  // val impl: function[StateT[ErrorF, Γ, _]] =
  //   new function[StateT[ErrorF, Γ, _]]:

  // Types
  // extension [A] (domain: `Type.Expr: *`[A])
  //   def ==>[B](codomain: `Type.Expr: *`[B]): `Type.AppInfix: *→*→* * *`[Function1, A, B] =
  //     FUNCTION1(domain, codomain)

  // extension [A] (domain: `Type: *`[A])
  //   def ==>[B](codomain: `Type: *`[B]): `Type.AppInfix: *→*→* * *`[Function1, A, B] =
  //     FUNCTION1(domain, codomain)

  extension [A] (domain: `Type.Expr: *`[A])
    def ==>[B](codomain: `Type.Expr: *`[B]): `Type.AppInfix: *→*→* * *`[Function1, A, B] =
      FUNCTION1(domain, codomain)

  extension [A] (domain: `Type.Expr: *`[A])
    def ==>[F[_], B](codomain: `Type.Expr: *→* *`[F, B]): `Type.AppInfix: *→*→* * (*→* *)`[Function1, F, A, B] =
      FUNCTION1(domain, codomain)

  extension [G[_[_], _], H[_], A, B] (domain: `Type.Expr: *`[A])
    def ==>(codomain: `Type.Expr: (*→*)→*→* *→* *`[G, H, B]): `Type.AppInfix: *→*→* * ((*→*)→*→* *→* *)`[Function1, G, H, A, B] =
      FUNCTION1(domain, codomain)

  extension [G[_[_], _], H[_], I[_], A, B] (domain: `Type.Expr: *`[A])
    def ==>(codomain: `Type.Expr: *→* ((*→*)→*→* *→* *)`[I, G, H, B]): `Type.AppInfix: *→*→* * (*→* ((*→*)→*→* *→* *))`[Function1, G, H, I, A, B] =
      FUNCTION1(domain, codomain)

  extension [G[_], H[_], I[_[_], _], A, B] (domain: `Type.Expr: *→* *`[G, A])
    def ==>(codomain: `Type.Expr: (*→*)→*→* *→* *`[I, H, B]): `Type.AppInfix: *→*→* (*→* *) ((*→*)→*→* *→* *)`[Function1, G, H, I, A, B] =
      FUNCTION1(domain, codomain)

  extension [A, B] (domain: (`Type.Expr: *`[A], `Type.Expr: *`[B]))
    def ==>[C](codomain: `Type.Expr: *`[C]): `Type.AppInfix: *→*→*→* * * *`[Function2, A, B, C] =
      FUNCTION2(domain._1, domain._2, codomain)

  extension [A, B, C] (domain: (`Type.Expr: *`[A], `Type.Expr: *`[B], `Type.Expr: *`[C]))
    def ==>[D](codomain: `Type.Expr: *`[D]): `Type.AppInfix: *→*→*→*→* * * * *`[Function3, A, B, C, D] =
      FUNCTION3(domain._1, domain._2, domain._3, codomain)

  // Values
  extension [A] (domain: `Value.Val: *`[A])
    def ==>[B](codomain: `Value.Val: *`[A] => `Value.Expr: *`[B]): `Value.Lam.1: *→*→* * *`[A, B] =
      function1(domain, codomain(domain))