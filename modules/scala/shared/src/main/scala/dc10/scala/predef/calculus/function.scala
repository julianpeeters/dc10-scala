package dc10.scala.predef.calculus

import dc10.scala.*
// trait function[F[_]]:

  // // Types
  // extension [A] (domain: `Type: x`[A])
  //   def ==>[B](codomain: `Type: x`[B]): `Type.AppInfix[_, _]`[Function1, A, B]

  // extension [A, B] (domain: (`Type: x`[A], `Type: x`[B]))
  //   def ==>[C](codomain: `Type: x`[C]): `Type.AppInfix[_, _, _]`[Function2, A, B, C]

  // extension [A, B, C] (domain: (`Type: x`[A], `Type: x`[B], `Type: x`[C]))
  //   def ==>[D](codomain: `Type: x`[D]): `Type.AppInfix[_, _, _, _]`[Function3, A, B, C, D]

  // // Values
  // extension [A] (domain: F[`Value.Var.Unbound.Data`[A]])
  //   def ==>[B](codomain: `Value.Var.Unbound.Data`[A] => `Value: x`[B]): F[`Value.Lam.1: x→x→x x x`[A, B]]

  // extension [A, B] (domain: (F[`Value.Var.Unbound.Data`[A]], F[`Value.Var.Unbound.Data`[B]]))
  //   def ==>[C](codomain: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B]) => `Value: x`[C]): F[`Value.Lam.2: x→x→x→x x * x`[A, B, C]]

  // extension [A, B, C] (domain: (F[`Value.Var.Unbound.Data`[A]], F[`Value.Var.Unbound.Data`[B]], F[`Value.Var.Unbound.Data`[C]]))
  //   def ==>[D](codomain: (`Value.Var.Unbound.Data`[A], `Value.Var.Unbound.Data`[B], `Value.Var.Unbound.Data`[C]) => `Value: x`[D]): F[`Value.Lam.3: x→x→x→x→x x * x x`[A, B, C, D]]

object function:

  // Types
  // def FUNCTION1[A, B](a: `Type: x`[A], b: `Type: x`[B]): `Type.AppInfix: x→x→x x x`[Function1, A, B] =
  //   `Type.AppInfix: x→x→x x x`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[A, B](a: `Type: x`[A], b: `Type: x`[B]): `Type.AppInfix: x→x→x x x`[Function1, A, B] =
    `Type.AppInfix: x→x→x x x`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[F[_], A, B](a: `Type: x`[A], b: `Type: x→x x`[F, B]): `Type.AppInfix: x→x→x x (x→x x)`[Function1, F, A, B] =
    `Type.AppInfix: x→x→x x (x→x x)`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[H[_], I[_[_], _], A, B](a: `Type: x`[A], b: `Type: (x→x)→x→x x→x x`[I, H, B]): `Type.AppInfix: x→x→x x ((x→x)→x→x x→x x)`[Function1, I, H, A, B] =
    `Type.AppInfix: x→x→x x ((x→x)→x→x x→x x)`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[G[_[_], _], H[_], I[_], A, B](a: `Type: x`[A], b: `Type: x→x ((x→x)→x→x x→x x)`[I, G, H, B]): `Type.AppInfix: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, I, A, B] =
    `Type.AppInfix: x→x→x x (x→x ((x→x)→x→x x→x x))`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  // def FUNCTION1[F[_], A, B](a: `Type: x`[A], b: `Type: x→x x`[F, B]): `Type.AppInfix: x→x→x x (x→x x)`[Function1, F, A, B] =
  //   `Type.AppInfix: x→x→x x (x→x x)`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)


  def FUNCTION1[G[_], H[_], I[_[_], _], A, B](a: `Type: x→x x`[G, A], b: `Type: (x→x)→x→x x→x x`[I, H, B]): `Type.AppInfix: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B] =
    `Type.AppInfix: x→x→x (x→x x) ((x→x)→x→x x→x x)`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  def FUNCTION1[T[_[_], _], F[_], A, I[_[_], _], J[_], K[_[_]], L[_], B](a: `Type: (x→x)→x→x x→x x`[T, F, A], b: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]): `Type.AppInfix: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, T, F, A, I, J, K, L] =
    `Type.AppInfix: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)


  // extension [T[_[_], _], F[_], A] (domain: `Type: (x→x)→x→x x→x x`[T, F, A])
  //   def ==>[I[_[_], _], J[_], K[_[_]], L[_], B](
  //     codomain: `Type: (x→x)→x→x x→x x`[T, F, A] => `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
  //   ): `Type.AppInfix: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, T, F, A, I, J, K, L] =
  //     FUNCTION1(domain, codomain)

  // def FUNCTION1[F[_], A, B](a: `Type: x`[A], b: `Type: x→x x`[F, B]): `Type.AppInfix[A, F[B]]`[Function1, F, A, B] =
  //   `Type.AppInfix[A, F[B]]`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)  

  // def FUNCTION1[T[_[_], _], F[_], A, B](a: `Type: x→x x`[F, A], b: `Type: (x→x)→x→x x→x x`[T, F, B]): `Type.AppInfix: x→x→x x x`[Function1, F[A], B] =
  //   `Type.AppInfix[F[A], G[_[_], _]]`(0, `Type.Var: x→x→x`(0, AliasSym("=>"), scala.None), a, b)

  // def FUNCTION2[A, B, C](a: `Type: x`[A], b: `Type: x`[B], c: `Type: x`[C]): `Type.AppInfix: x→x→x→x x * x`[Function2, A, B, C] =
  //   `Type.AppInfix: x→x→x→x x * x`(0, `Type.Var: x→x→x→x`(0, AliasSym("=>"), scala.None), a, b, c)

  // def FUNCTION3[A, B, C, D](a: `Type: x`[A], b: `Type: x`[B], c: `Type: x`[C], d: `Type: x`[D]): `Type.AppInfix: x→x→x→x→x x * x x`[Function3, A, B, C, D] =
  //   `Type.AppInfix: x→x→x→x→x x * x x`(0, `Type.Var: x→x→x→x→x`(0, AliasSym("=>"), scala.None), a, b, c, d)
  
  // Values
  def function1[A, B](a: `Value: x`[A], b: `Value: x`[B]): `Value.Lam.1: x→x→x x x`[A, B] =
    `Value.Lam.1: x→x→x x x`(0, a, b, FUNCTION1(a.tpe, b.tpe))

  def function1[T[_[_], _], F[_], A, I[_[_], _], J[_], K[_[_]], L[_], B](a: `Value.Val: (x→x)→x→x x→x x`[T, F, A], b: `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]): `Value.Lam1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, T, F, A, I, J, K, L] =
    `Value.Lam1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`(0, a, b, FUNCTION1(a.tpe, b.tpe))

  // def function2[A, B, C](a: `Value: x`[A], b: `Value: x`[B], c: `Value: x`[C]): `Value.Lam.2: x→x→x→x x * x`[A, B, C] =
  //   `Value.Lam.2: x→x→x→x x * x`(0, a, b, c, FUNCTION2(a.tpe, b.tpe, c.tpe))

  // def function3[A, B, C, D](a: `Value: x`[A], b: `Value: x`[B], c: `Value: x`[C], d: `Value: x`[D]): `Value.Lam.3: x→x→x→x→x x * x x`[A, B, C, D] =
  //   `Value.Lam.3: x→x→x→x→x x * x x`(0, a, b, c, d, FUNCTION3(a.tpe, b.tpe, c.tpe, d.tpe))

  // Impl
  // val impl: function[StateT[ErrorF, Γ, _]] =
  //   new function[StateT[ErrorF, Γ, _]]:

  // Types
  // extension [A] (domain: `Type: x`[A])
  //   def ==>[B](codomain: `Type: x`[B]): `Type.AppInfix: x→x→x x x`[Function1, A, B] =
  //     FUNCTION1(domain, codomain)

  // extension [A] (domain: `Type: x`[A])
  //   def ==>[B](codomain: `Type: x`[B]): `Type.AppInfix: x→x→x x x`[Function1, A, B] =
  //     FUNCTION1(domain, codomain)

  extension [A] (domain: `Type: x`[A])
    def ==>[B](codomain: `Type: x`[B]): `Type.AppInfix: x→x→x x x`[Function1, A, B] =
      FUNCTION1(domain, codomain)

  extension [A] (domain: `Type: x`[A])
    def ==>[F[_], B](codomain: `Type: x→x x`[F, B]): `Type.AppInfix: x→x→x x (x→x x)`[Function1, F, A, B] =
      FUNCTION1(domain, codomain)

  // extension [G[_[_], _], H[_], A, B] (domain: `Type: x`[A])
  //   def ==>(codomain: `Type: (x→x)→x→x x→x x`[G, H, B]): `Type.AppInfix: x→x→x x ((x→x)→x→x x→x x)`[Function1, G, H, A, B] =
  //     FUNCTION1(domain, codomain)

  extension [G[_[_], _], H[_], I[_], A, B] (domain: `Type: x`[A])
    def ==>(codomain: `Type: x→x ((x→x)→x→x x→x x)`[I, G, H, B]): `Type.AppInfix: x→x→x x (x→x ((x→x)→x→x x→x x))`[Function1, G, H, I, A, B] =
      FUNCTION1(domain, codomain)

  // extension [F[_], A, B] (domain: `Type: x`[A])
  //   def ==>(codomain: `Type: x→x x`[F, B]): `Type.AppInfix: x→x→x x (x→x x)`[Function1, F, A, B] =
  //     FUNCTION1(domain, codomain)

  extension [G[_], H[_], I[_[_], _], A, B] (domain: `Type: x→x x`[G, A])
    def ==>(codomain: `Type: (x→x)→x→x x→x x`[I, H, B]): `Type.AppInfix: x→x→x (x→x x) ((x→x)→x→x x→x x)`[Function1, G, H, I, A, B] =
      FUNCTION1(domain, codomain)

  extension [T[_[_], _], F[_], A] (domain: `Type: (x→x)→x→x x→x x`[T, F, A])
    def ==>[I[_[_], _], J[_], K[_[_]], L[_], B](
      codomain: `Type: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
    ): `Type.AppInfix: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, T, F, A, I, J, K, L] =
      FUNCTION1(domain, codomain)

  // extension [A, B] (domain: (`Type: x`[A], `Type: x`[B]))
  //   def ==>[C](codomain: `Type: x`[C]): `Type.AppInfix: x→x→x→x x * x`[Function2, A, B, C] =
  //     FUNCTION2(domain._1, domain._2, codomain)

  // extension [A, B, C] (domain: (`Type: x`[A], `Type: x`[B], `Type: x`[C]))
  //   def ==>[D](codomain: `Type: x`[D]): `Type.AppInfix: x→x→x→x→x x * x x`[Function3, A, B, C, D] =
  //     FUNCTION3(domain._1, domain._2, domain._3, codomain)

  // Values
  extension [A] (domain: `Value.Val: x`[A])
    def ==>[B](codomain: `Value.Val: x`[A] => `Value: x`[B]): `Value.Lam.1: x→x→x x x`[A, B] =
      function1(domain, codomain(domain))

  extension [T[_[_], _], F[_], A] (domain: `Value.Val: (x→x)→x→x x→x x`[T, F, A])
    def ==>[I[_[_], _], J[_], K[_[_]], L[_], B](
      codomain: `Value.Val: (x→x)→x→x x→x x`[T, F, A] => `Value: (x→x)→x→x x→x ((x→x)→x x→x)`[I, J, K, L]
    ): `Value.Lam1: x→x→x ((x→x)→x→x x→x x) ((x→x)→x→x x→x ((x→x)→x x→x))`[Function1, T, F, A, I, J, K, L] =
      function1(domain, codomain(domain))