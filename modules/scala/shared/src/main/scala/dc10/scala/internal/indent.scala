package dc10.scala.internal

import dc10.scala.*

object indent:

  extension (statement: Statement)
    def addIndent: Statement =
      statement match
        case s@Statement.`case class`(tpe, fields, body) => s.copy(tpe = tpe.addIndent, fields.map(f => f.addIndent), body = body.map(m => m.addIndent))
        case s@Statement.`extension`(field, body) => s.copy(field = field.addIndent, body.map(m => m.addIndent))
        case s@Statement.`def`.`0`(value) => s.copy(value = value.addIndent)
        case s@Statement.`def`.`0`.`[_]`(tparam, impl, value) => s.copy(tparam = tparam.addIndent, impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`def`.`0`.`[_[_]]`(tparam, impl, value) => s.copy(tparam = tparam.addIndent, impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => s.copy(tparamf = tparamf.addIndent, tparama = tparama.addIndent, impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`def`.`1`(arg, ret, impl, value) => s.copy(arg = arg.addIndent, ret = ret.addIndent, impl = impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`def`.`1`.`[_]`(tparam, arg, ret, impl, value) => s.copy(tparam = tparam.addIndent, arg = arg.addIndent, ret = ret.addIndent, impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`def`.`1`.`[_[_], _]`(tparamf, tparama, arg, ret, impl, value) => s.copy(tparamf = tparamf.addIndent, tparama = tparama.addIndent, arg = arg.addIndent, ret = ret.addIndent, impl.map(i => i.addIndent), value = value.addIndent)
        case s@Statement.`field`(value) => s.copy(value = value.addIndent)
        case s@Statement.`generator`(value) => s.copy(value = value.addIndent)
        case s@Statement.`object`(value, parent, body) => s.copy(value = value.addIndent, body = body.map(m => m.addIndent))
        case s@Statement.`package`(nme, contents) => s.copy(contents = contents.map(s => s.addIndent))
        case s@Statement.`trait`(tpe, parent, body) => s.copy(tpe = tpe.addIndent, body = body.map(s => s.addIndent))
        case s@Statement.`trait`.`[_]`(tpe, tparam, parent, body) => s.copy(tpe = tpe.addIndent, body = body.map(s => s.addIndent))
        case s@Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body) => s.copy(tpe = tpe.addIndent, tparam = tparam.addIndent, body = body.map(s => s.addIndent))
        case s@Statement.`trait`.`[_[_], _]`(tpe, tparamf, tparama, parent, body) => s.copy(tpe = tpe.addIndent, tparamf = tparamf.addIndent, tparama = tparama.addIndent, body = body.map(s => s.addIndent))
        case s@Statement.`type`(tpe) => s.copy(tpe = tpe.addIndent)
        case s@Statement.`type`.`[_]`(tparam, tpe) => s.copy(tparam = tparam.addIndent, tpe = tpe.addIndent)
        case s@Statement.`type`.`[_]=>>`(tpe) => s.copy(tpe.addIndent)
        case s@Statement.`type`.`[_[_]]`(tparam, tpe) => s.copy(tparam = tparam.addIndent, tpe = tpe.addIndent)
        case s@Statement.`type`.`[_[_], _]`(tparamf, tparama, tpe) => s.copy(tparamf = tparamf.addIndent, tparama = tparama.addIndent, tpe = tpe.addIndent)
        case s@Statement.`val`(value) => s.copy(value = value.addIndent)
    
    def getIndent: Int =
      statement match
        case Statement.`case class`(tpe, fields, body)                          => tpe.in
        case Statement.`extension`(field, body)                                 => field.in
        case Statement.`def`.`0`(value)                                         => value.getIndent
        case Statement.`def`.`0`.`[_]`(tparam, impl, value)                     => value.getIndent
        case Statement.`def`.`0`.`[_[_]]`(tparam, impl, value)                  => value.getIndent
        case Statement.`def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value)     => value.getIndent
        case Statement.`def`.`1`(arg, impl, ret, value)                         => value.getIndent
        case Statement.`def`.`1`.`[_]`(tparam, arg, impl, ret, value)           => value.getIndent
        case Statement.`def`.`1`.`[_[_], _]`(f, a, arg, impl, ret, value)       => value.getIndent
        case Statement.`field`(value)                                           => value.getIndent
        case Statement.`generator`(value)                                       => value.in
        case Statement.`object`(value, parent, body)                            => value.in
        case Statement.`package`(nme, contents)                                 => 0
        case Statement.`trait`(tpe, parent, body)                               => tpe.in
        case Statement.`trait`.`[_]`(tpe, tparam, parent, body)                 => tpe.in
        case Statement.`trait`.`[_[_]]`(tpe, tparam, parent, body)              => tpe.in
        case Statement.`trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => tpe.in
        case Statement.`type`(tpe)                                              => tpe.in
        case Statement.`type`.`[_]`(tparam, tpe)                                => tpe.in
        case Statement.`type`.`[_]=>>`(tpe)                                     => tpe.in
        case Statement.`type`.`[_[_]]`(tparam, tpe)                             => tpe.in
        case Statement.`type`.`[_[_], _]`(tparamF, tparamA, tpe)                => tpe.in
        case Statement.`val`(value)                                             => value.getIndent

  extension [A] (term: `Type.Var`[A])
    def addIndent: `Type.Var`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [F[_]] (term: `Type.Var[_]`[F])
    def addIndent: `Type.Var[_]`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [F[_[_]]] (term: `Type.Var[_[_]]`[F])
    def addIndent: `Type.Var[_[_]]`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in  

  extension [F[_[_], _]] (term: `Type.Var[_[_], _]`[F])
    def addIndent: `Type.Var[_[_], _]`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [A] (term: `Type.*`[A])
    def addIndent: `Type.*`[A] =
      term match
        case t@`Type.App[_]`(in, tfun, aarg) =>  t.copy(in = in + 1)
        case t@`Type.App[_[_]]`(in, tfun, farg) => t.copy(in = in + 1)
        case t@`Type.App[_[_[_], _]]`(in, tfun, farg) => t.copy(in = in + 1)
        case t@`Type.App[_, _]`(in, tfun, aarg, barg) => t.copy(in = in + 1)
        case t@`Type.App[_[_], _]`(in, tfun, farg, aarg) => t.copy(in = in + 1)
        case t@`Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => t.copy(in = in + 1)
        case t@`Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => t.copy(in = in + 1)
        case t@`Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => t.copy(in = in + 1)
        case t@`Type.AppInfix[_, _]`(in, tfun, aarg, barg) => t.copy(in = in + 1)
        case t@`Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => t.copy(in = in + 1)
        case t@`Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => t.copy(in = in + 1)
        case t@`Type.Var`(in, nme, impl) => t.copy(in = in + 1)
        case t@`Type.Bot`(in) => t.copy(in = in + 1)

    def getIndent: Int =
      term match
        case `Type.App[_]`(in, tfun, aarg) => in
        case `Type.App[_[_]]`(in, tfun, farg) => in
        case `Type.App[_[_[_], _]]`(in, tfun, farg) => in
        case `Type.App[_, _]`(in, tfun, aarg, barg) => in
        case `Type.App[_[_], _]`(in, tfun, farg, aarg) => in
        case `Type.App[_, _, _]`(in, tfun, aarg, barg, carg) => in
        case `Type.App[_[_], _, _]`(in, tfun, farg, aarg, barg) => in
        case `Type.App[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => in
        case `Type.AppInfix[_, _]`(in, tfun, aarg, barg) => in
        case `Type.AppInfix[_, _, _]`(in, tfun, aarg, barg, carg) => in
        case `Type.AppInfix[_, _, _, _]`(in, tfun, aarg, barg, carg, darg) => in
        case `Type.Var`(in, nme, impl) => in
        case `Type.Bot`(in) => in

  extension [F[_]] (term: `Type.*->*`[F])
    def addIndent: `Type.*->*`[F] =
      term match
        case t@`Type.Lam`(in, domain, codomain) => t.copy(in = in + 1)
        case t@`Type.Var[_]`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Lam`(in, domain, codomain) => in
        case `Type.Var[_]`(in, nme, impl) => in
      
  extension [F[_, _]] (term: `Type.*->*->*`[F])
    def addIndent: `Type.*->*->*`[F] =
      term match
        case t@`Type.Var[_, _]`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var[_, _]`(in, nme, impl) => in
        
  extension [F[_[_], _]] (term: `Type.(*->*)->*->*`[F])
    def addIndent: `Type.(*->*)->*->*`[F] =
      term match
        case t@`Type.Var[_[_], _]`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var[_[_], _]`(in, nme, impl) => in
      
  extension [F[_, _, _]] (term: `Type.*->*->*->*`[F])
    def addIndent: `Type.*->*->*->*`[F] =
      term match
        case t@`Type.Var[_, _, _]`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var[_, _, _]`(in, nme, impl) => in 

  extension [F[_[_], _, _]] (term: `Type.(*->*)->*->*->*`[F])
    def addIndent: `Type.(*->*)->*->*->*`[F] =
      term match
        case t@`Type.Var[_[_], _, _]`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var[_[_], _, _]`(in, nme, impl) => in

  extension [A] (term: `Value.Var`[A])
    def addIndent: `Value.Var`[A] =
      term match
        case t@`Value.VarA`(in, nme, tpe) => t.copy(in = in + 1)
        case t@`Value.VarC`(in, nme, tpe, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Value.VarA`(in, nme, tpe) => in
        case `Value.VarC`(in, nme, tpe, impl) => in

  extension [A] (term: `Value.VarA`[A])
    def addIndent: `Value.VarA`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [A] (term: `Value.VarC`[A])
    def addIndent: `Value.VarC`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in
      
  extension [A] (term: `Value.*`[A])
    def addIndent: `Value.*`[A] =
      term match
        case v@`Value.App1`(in, fun, arg, tpe) => v.copy(in = in + 1)
        case v@`Value.App2`(in, fun, arg, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.App3`(in, fun, arg, arg2, arg3, tpe) => v.copy(in = in + 1)
        case `Value.AppVargs`(in, fun, tpe, vargs*) => `Value.AppVargs`(in + 1, fun, tpe, vargs*)
        case v@`Value.AppDot0`(in, fun, arg1, tpe) => v.copy(in = in + 1)
        case v@`Value.AppDot1`(in, fun, arg1, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.AppDotless`(in, fun, arg1, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.AppForComp`(in, gens, ret, tpe) => v.copy(in = in + 1, gens.map(s => s.addIndent))
        case v@`Value.AppType`(in, fun, targ, tpe) => v.copy(in = in + 1)
        case v@`Value.LitBoolean`(in, tpe, b) => v.copy(in = in + 1)
        case v@`Value.LitInt`(in, tpe, i) => v.copy(in = in + 1)
        case v@`Value.LitString`(in, tpe, s) => v.copy(in = in + 1)
        case v@`Value.LitUnit`(in, tpe, u) => v.copy(in = in + 1)
        case v@`Value.Lam1`(in, a, b, tpe) => v.copy(in = in + 1)
        case v@`Value.Lam2`(in, a1, a2, r, tpe) => v.copy(in = in + 1)
        case v@`Value.Lam3`(in, a1, a2, a3, r, tpe) => v.copy(in = in + 1)
        case v@`Value.VarA`(in, nme, tpe) => v.copy(in = in + 1)
        case v@`Value.VarC`(in, nme, tpe, impl) => v.copy(in = in + 1)
      
    def getIndent: Int =
      term match
        case `Value.App1`(in, fun, arg, tpe) => in
        case `Value.App2`(in, fun, arg, arg2, tpe) => in
        case `Value.App3`(in, fun, arg, arg2, arg3, tpe) => in
        case `Value.AppVargs`(in, fun, tpe, vargs*) => in
        case `Value.AppDot0`(in, fun, arg1, tpe) => in
        case `Value.AppDot1`(in, fun, arg1, arg2, tpe) => in
        case `Value.AppDotless`(in, fun, arg1, arg2, tpe) => in
        case `Value.AppForComp`(in, gens, ret, tpe) => in
        case `Value.AppType`(in, fun, targ, tpe) => in
        case `Value.LitBoolean`(in, tpe, b) => in
        case `Value.LitInt`(in, tpe, i) => in
        case `Value.LitString`(in, tpe, s) => in
        case `Value.LitUnit`(in, tpe, u) => in
        case `Value.Lam1`(in, a, b, tpe) => in
        case `Value.Lam2`(in, a1, a2, r, tpe) => in
        case `Value.Lam3`(in, a1, a2, a3, r, tpe) => in
        case `Value.VarA`(in, nme, tpe) => in
        case `Value.VarC`(in, nme, tpe, impl) => in
      
  extension [F[_]] (term: `Value.*->*`[F])
    def addIndent: `Value.*->*`[F] =
      term match
        case v@`Value.Var0[_]`(in, nme, tpe, impl) => v.copy(in = in +1)
        case v@`Value.Var1[_]`(in, nme, tpe, impl) => v.copy(in = in +1)
    def getIndent: Int =
      term match
        case `Value.Var0[_]`(in, nme, tpe, impl) => in
        case `Value.Var1[_]`(in, nme, tpe, impl) => in