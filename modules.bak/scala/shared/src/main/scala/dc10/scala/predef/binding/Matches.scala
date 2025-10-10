package dc10.scala.predef.binding

import cats.data.StateT
import cats.syntax.all.toTraverseOps
import dc10.scala.*
import dc10.scala.internal.indent.addIndent

import dc10.scala.internal.extract.unpure
import dc10.scala.predef.calculus.Functions.function1

trait Matches[F[_]]:

  extension [A] (v: F[`Value: x`[A]])
    infix def MATCH[B](
      cases: F[Statement.`case`[A => B]]*
    ): F[`Value: x`[B]]
  
  def CASE[T, B](
    lhs: F[`Value.Var`[T]],
    rhs: F[`Value: x`[B]]
  ): F[Statement.`case`[T => B]]

  def CASE[T, A, B, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](
    lhs: F[`Value.App.1: x`[A, T, `T.x`, `V.x`]],
    rhs: `Value: x`[A] => F[`Value: x`[B]]
  ): F[Statement.`case`[T => B]]


object Matches:

  trait Mixins extends Matches[StateT[ErrorF, Γ, _]]:

    extension [A] (v: StateT[ErrorF, Γ, `Value: x`[A]])
      infix def MATCH[B](
        cases: StateT[ErrorF, Γ, Statement.`case`[A => B]]*
      ): StateT[ErrorF, Γ, `Value: x`[B]] =
        for
          a <- StateT.liftF(v.runEmptyA)
          l <- cases.toList.sequence
          t <- StateT.liftF(l.headOption.fold(Left(List(Error("Empty match cases"))))(c => c.lambda.unpure))
        yield `Value.App.Match`(0, a, t.tpe, l.map(s => s.copy(lambda = s.lambda.addIndent)))

    def CASE[T, B](
      lhs: StateT[ErrorF, Γ, `Value.Var`[T]],
      rhs: StateT[ErrorF, Γ, `Value: x`[B]]
    ): StateT[ErrorF, Γ, Statement.`case`[T => B]] =
      for
        a <- StateT.liftF(lhs.runEmptyA)
        b <- StateT.liftF(rhs.runEmptyA)
      yield Statement.`case`(function1(a, b))

    def CASE[T, A, B, `T.x`[t] <: `Type: x`[t], `V.x`[t] <: `Value: x`[t]](
      lhs: StateT[ErrorF, Γ, `Value.App.1: x`[A, T, `T.x`, `V.x`]],
      rhs: `Value: x`[A] => StateT[ErrorF, Γ, `Value: x`[B]]
    ): StateT[ErrorF, Γ, Statement.`case`[T => B]] =
      for
        a <- StateT.liftF(lhs.runEmptyA)
        r <- StateT.pure(a.arg)
        b <- StateT.liftF(rhs(r).runEmptyA)
      yield Statement.`case`(function1(a, b))