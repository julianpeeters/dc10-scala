package dc10.scala.internal

import dc10.scala.*
import dc10.scala.internal.construct.ctor
import dc10.scala.internal.indent.addIndent

object implement:

  extension (statement: Statement)
    def getValue[T]: Either[List[Error], `Value.*`[T]] =
      statement match
        case Statement.`case class`(tpe, fields, body) => Left(List(Error("Not a value level definition"))) 
        case Statement.`extension`(field, body) => Left(List(Error("Not a value level definition")))
        case Statement.`def`.`0`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`0`.`[_]`(tparam, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`1`(arg, ret, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`def`.`1`.`[_[_], _]`(tparamf, tparama, arg, impl, ret, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`field`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`generator`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`object`(value, parent, body) => Right(value.asInstanceOf[`Value.*`[T]])
        case Statement.`package`(nme, contents) => Left(List(Error("Not a value level definition")))
        case Statement.`trait`(tpe, parent, body) => Left(List(Error("Not a value level definition")))
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body) => Left(List(Error("Not a value level definition")))
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => Left(List(Error("Not a value level definition")))
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => Left(List(Error("Not a value level definition")))
        case Statement.`type`(tpe) => Left(List(Error("Not a value level definition")))
        case Statement.`type`.`[_]`(tparam, tpe) => Left(List(Error("Not a value level definition")))
        case Statement.`type`.`[_]=>>`(tpe) => Left(List(Error("Not a value level definition")))
        case Statement.`type`.`[_[_]]`(tparam, tpe) => Left(List(Error("Not a value level definition")))
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe) => Left(List(Error("Not a value level definition")))
        case Statement.`val`(value) => Right(value.asInstanceOf[`Value.*`[T]])

    def assign[T](rhs: `Value.*`[T]): Either[List[Error], (Statement, `Value.*`[T])] =
      statement match
        case Statement.`case class`(tpe, fields, body) => Left(List(Error("Not a declaration *")))
        case Statement.`extension`(field, body) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`(value) => value match
          case `Value.VarA`(in, nme, tpe) => Right(`Value.VarC`[T](in, nme, rhs.tpe, rhs)).map(v => (Statement.`def`.`0`(v), v))
          case `Value.VarC`(in, nme, tpe, impl) => Left(List(Error(s"Already defiend * ${value}")))
        case Statement.`def`.`0`.`[_]`(tparam, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`1`(arg, ret, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`1`.`[_[_], _]`(tparamf, tparama, arg, impl, ret, value) => Left(List(Error("Not a declaration *")))
        case Statement.`field`(value) => Left(List(Error("Not a declaration *")))
        case Statement.`generator`(value) => Left(List(Error("Not a declaration *")))
        case Statement.`object`(value, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`package`(nme, contents) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`(tpe, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`type`(tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_]`(tparam, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_]=>>`(tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_[_]]`(tparam, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`val`(value) =>
          value match
            case `Value.VarA`(in, nme, tpe) =>
              Right(`Value.VarC`[T](in, nme, tpe.asInstanceOf[`Type.*`[T]], rhs.addIndent)).map(v => (Statement.`val`(v), v))
            case `Value.VarC`(in, nme, tpe, impl) =>
              Left(List(Error(s"Already defiend * ${value}")))

    def assign[A, B](
      rhs: `Value.*`[A] => Either[List[Error], `Value.*`[B]]
    ): Either[List[Error], (Statement, `Value.*`[A => B])] =
      statement match
        case Statement.`case class`(tpe, fields, body) => Left(List(Error("Not a declaration *")))
        case Statement.`extension`(field, body) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`(value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`.`[_]`(tparam, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`1`(arg, ret, impl, value) =>
          value match
            case `Value.VarA`(in, nme, tpe) =>
              for
                a <- Right(arg.asInstanceOf[`Value.*`[A]])
                r <- rhs(a)
                f <- Right(r.ctor(a))
                v <- Right(`Value.VarC`(in, nme, f.tpe, f.addIndent))
              yield (Statement.`def`.`1`(a, r.tpe, Some(r), v), v)
            case `Value.VarC`(in, nme, tpe, impl) => Left(List(Error(s"Already defined * ${value}")))
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value) => Left(List(Error("Not a declaration *")))
        case Statement.`def`.`1`.`[_[_], _]`(f, a, arg, impl, ret, value) => Left(List(Error("Not a declaration *")))
        case Statement.`field`(value) => Left(List(Error("Not a declaration *")))
        case Statement.`generator`(value) => Left(List(Error("Not a declaration *")))
        case Statement.`object`(value, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`package`(nme, contents) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`(tpe, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => Left(List(Error("Not a declaration *")))
        case Statement.`type`(tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_]`(tparam, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_]=>>`(tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_[_]]`(tparam, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe) => Left(List(Error("Not a declaration *")))
        case Statement.`val`(value) => Left(List(Error("Not a declaration *")))

  extension [T] (t: `Type.*`[T])
    def assign[A](rhs: `Type.*`[A]): Either[List[Error], `Type.Var`[A]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.Var`(in, nme, impl) => impl.fold(Right(`Type.Var`(in, nme, Some(rhs))))(_ => Left(List(Error(s"Type is already assigned ${t}"))))
        case `Type.Bot`(in) => Left(List(Error(s"Type is not assignable ${t}")))
    
    def assign[F[_]](rhs: `Type.*->*`[F]): Either[List[Error], `Type.Var[_]`[F]] =
      t match
        case `Type.App[_]`(in, tfun, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_]]`(in, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => Left(List(Error(s"Type is not assignable ${t}")))
        case `Type.Var`(in, nme, impl) => impl.fold(Right(`Type.Var[_]`(in, nme, Some(rhs))))(_ => Left(List(Error(s"Type is already assigned ${t}"))))
        case `Type.Bot`(in) => Left(List(Error(s"Type is not assignable ${t}")))
 
  extension [T] (value: `Value.*`[T])
    def findImpl: Option[`Value.*`[T]] =
      value match
        case `Value.App1`(in, fun, arg, tpe) => Some(value)
        case `Value.App2`(in, fun, arg1, arg2, tpe) => Some(value) 
        case `Value.App3`(in, fun, arg1, arg2, arg3, tpe) => Some(value) 
        case `Value.AppVargs`(in, fun, tpe, vargs*) => Some(value)
        case `Value.AppDot0`(in, fun, arg1, tpe) => Some(value)
        case `Value.AppDot1`(in, fun, arg1, arg2, tpe) => Some(value)
        case `Value.AppDotless`(in, fun, arg1, arg2, tpe) => Some(value)
        case `Value.AppForComp`(in, l, r, t) => Some(value)
        case `Value.AppType`(in, fun, targ, tpe) => Some(value)
        case `Value.Lam1`(in, a, b, t) => Some(value)
        case `Value.Lam2`(in, a1, a2, b, t) => Some(value)
        case `Value.Lam3`(in, a1, a2, a3, b, t) => Some(value)
        case `Value.LitBoolean`(in, tpe, b) => Some(value)
        case `Value.LitInt`(in, tpe, i) => Some(value)
        case `Value.LitString`(in, tpe, s) => Some(value)
        case `Value.LitUnit`(in, tpe, s) => Some(value)
        case `Value.VarA`(in, nme, tpe) => None
        case `Value.VarC`(in, nme, tpe, impl) => impl.findImpl