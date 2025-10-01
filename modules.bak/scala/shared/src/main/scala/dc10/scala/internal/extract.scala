package dc10.scala.internal

import dc10.scala.*
import dc10.scala.predef.datatype.PrimitiveTypes.unit

object extract:

  extension [A] (v: `Value.Expr: *`[A])
    def unapply[B]: Either[List[Error], `Value.Expr: *`[B]] =
      v match
        case `Value.App.1: *`(in, fun, arg, tpe) => Right(arg.asInstanceOf[`Value.Expr: *`[B]])
        case `Value.App.2: *`(in, fun, arg, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.3: *`(in, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.Vargs: *`(in, fun, tpe, vargs*) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDot.0: *`(in, fun, arg1, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDot.1: *`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDotless: *`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppForComp: *→* *`(in, gens, ret, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.Match`(in, value, tpe, cases) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: *→*`(in, fun, targ, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: (*→*)→*→*`(in, fun, targf, targa, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: (*→*)→*→*`(in, fun, targf, targa, targb, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Boolean: *`(in, tpe, b) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Int: *`(in, tpe, i) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.String: *`(in, tpe, s) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Unit: *`(in, tpe, u) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.1: *→*→* * *`(in, a, b, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.2: *→*→*→* * * *`(in, a1, a2, r, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.3: *→*→*→*→* * * * *`(in, a1, a2, a3, r, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Var.Unbound.Data`(in, nme, tpe) => Right(unit(()).asInstanceOf[`Value.Expr: *`[B]])
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => Left(List(Error(s"Value in not a construction ${v}")))
      

  extension [F[_], A] (t: `Type.Expr: *`[F[A]])
    def unpure: Either[List[Error], `Type.Expr: *`[A]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Right(aarg.asInstanceOf[`Type.Expr: *`[A]])
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Right(barg.asInstanceOf[`Type.Expr: *`[A]])
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Var: *`(in, nme, impl) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Bot: *`(in) => Left(List(Error(s"Type is not a functor ${t}")))

  extension [F[_], A] (v: `Value.Expr: *`[F[A]])
    def unpure: Either[List[Error], `Value.Expr: *`[A]] =
      v match
        case `Value.App.1: *`(in, fun, arg, tpe) => Right(arg.asInstanceOf[`Value.Expr: *`[A]])
        case `Value.App.2: *`(in, fun, arg, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.3: *`(in, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.Vargs: *`(in, fun, tpe, vargs*) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot.0: *`(in, fun, arg1, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot.1: *`(in, fun, arg1, arg2, tpe) => Right(arg2.asInstanceOf[`Value.Expr: *`[A]])
        case `Value.AppDotless: *`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppForComp: *→* *`(in, gens, ret, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.Match`(in, value, tpe, cases) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: *→*`(in, fun, targ, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: (*→*)→*→*`(in, fun, targf, targa, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: (*→*)→*→*`(in, fun, targf, targa, targb, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Boolean: *`(in, tpe, b) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Int: *`(in, tpe, i) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.String: *`(in, tpe, s) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Unit: *`(in, tpe, u) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam.1: *→*→* * *`(in, a, b, tpe) => Right(b.asInstanceOf[`Value.Expr: *`[A]])
        case `Value.Lam.2: *→*→*→* * * *`(in, a1, a2, c, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam.3: *→*→*→*→* * * * *`(in, a1, a2, a3, r, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Var.Unbound.Data`(in, nme, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => Left(List(Error(s"not a FlatMap value ${v}")))