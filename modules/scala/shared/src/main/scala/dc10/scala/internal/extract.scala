package dc10.scala.internal

import dc10.scala.*

object extract:

  extension [F[_], A] (t: `Type.*`[F[A]])
    def unpure: Either[List[Error], `Type.*`[A]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Right(aarg.asInstanceOf[`Type.*`[A]])
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Right(barg.asInstanceOf[`Type.*`[A]])
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Var`(in, nme, impl) => Left(List(Error(s"Type is not a functor ${t}")))
        case `Type.Bot`(in) => Left(List(Error(s"Type is not a functor ${t}")))

  extension [F[_], A] (v: `Value.*`[F[A]])
    def unpure: Either[List[Error], `Value.*`[A]] =
      v match
        case `Value.App1`(in, fun, arg, tpe) => Right(arg.asInstanceOf[`Value.*`[A]])
        case `Value.App2`(in, fun, arg, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.App3`(in, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppVargs`(in, fun, tpe, vargs*) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot0`(in, fun, arg1, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppDot1`(in, fun, arg1, arg2, tpe) => Right(arg2.asInstanceOf[`Value.*`[A]])
        case `Value.AppDotless`(in, fun, arg1, arg2, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppForComp`(in, gens, ret, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.AppType`(in, fun, targ, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.LitBoolean`(in, tpe, b) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.LitInt`(in, tpe, i) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.LitString`(in, tpe, s) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.LitUnit`(in, tpe, u) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam1`(in, a, b, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam2`(in, a1, a2, c, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.Lam3`(in, a1, a2, a3, r, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.VarA`(in, nme, tpe) => Left(List(Error(s"not a FlatMap value ${v}")))
        case `Value.VarC`(in, nme, tpe, impl) => Left(List(Error(s"not a FlatMap value ${v}")))