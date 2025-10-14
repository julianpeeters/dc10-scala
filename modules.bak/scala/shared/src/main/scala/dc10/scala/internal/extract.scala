package dc10.scala.internal

import dc10.scala.*
import dc10.scala.predef.datatype.PrimitiveTypes.unit

object extract:

  extension [A] (v: `Value: x`[A])
    def unapply[B]: Either[List[Error], `Value: x`[B]] =
      v match
        case `Value.App.1: x`(in, fun, arg, tpe) => Right(arg.asInstanceOf[`Value: x`[B]])
        case `Value.App.2: x`(in, fun, arg, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.3: x`(in, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.Vargs: x`(in, fun, tpe, vargsxl => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDot.0: x`(in, fun, arg1, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDot.1: x`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppDotless: x`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.AppForComp: x_x x`(in, gens, ret, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.Match`(in, value, tpe, cases) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: x_x`(in, fun, targ, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, targb, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Boolean: x`(in, tpe, b) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Int: x`(in, tpe, i) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.String: x`(in, tpe, s) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lit.Unit: x`(in, tpe, u) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.1: x_x_x x x`(in, a, b, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.2: x_x_x_x x * x`(in, a1, a2, r, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Lam.3: x_x_x_x_x x * x x`(in, a1, a2, a3, r, tpe) => Left(List(Error(s"Value in not a construction ${v}")))
        case `Value.Var.Unbound.Data`(in, nme, tpe) => Right(unit(()).asInstanceOf[`Value: x`[B]])
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => Left(List(Error(s"Value in not a construction ${v}")))
      

  extension [F[_], A] (t: `Type: x`[F[A]])
    def unpure: Either[List[Error], `Type: x`[A]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Right(aarg.asInstanceOf[`Type: x`[A]])
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Right(barg.asInstanceOf[`Type: x`[A]])
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Var: x`(in, nme, impl) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Bot: x`(in) => Left(List(Error(s"Type is not a functor ${t}")))

  extension [F[_], A] (v: `Value: x`[F[A]])
    def unpure: Either[List[Error], `Value: x`[A]] =
      v match
        case `Value.App.1: x`(in, fun, arg, tpe) => Right(arg.asInstanceOf[`Value: x`[A]])
        case `Value.App.2: x`(in, fun, arg, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.3: x`(in, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.Vargs: x`(in, fun, tpe, vargsxl => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot.0: x`(in, fun, arg1, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot.1: x`(in, fun, arg1, arg2, tpe) => Right(arg2.asInstanceOf[`Value: x`[A]])
        case `Value.AppDotless: x`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppForComp: x_x x`(in, gens, ret, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.Match`(in, value, tpe, cases) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: x_x`(in, fun, targ, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, targb, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Boolean: x`(in, tpe, b) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Int: x`(in, tpe, i) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.String: x`(in, tpe, s) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lit.Unit: x`(in, tpe, u) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam.1: x_x_x x x`(in, a, b, tpe) => Right(b.asInstanceOf[`Value: x`[A]])
        case `Value.Lam.2: x_x_x_x x * x`(in, a1, a2, c, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam.3: x_x_x_x_x x * x x`(in, a1, a2, a3, r, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Var.Unbound.Data`(in, nme, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => Left(List(Error(s"not a FlatMap value ${v}")))