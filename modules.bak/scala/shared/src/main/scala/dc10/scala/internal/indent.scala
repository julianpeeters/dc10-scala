package dc10.scala.internal

import dc10.scala.*

object indent:

  extension (statement: Statement)
    def addIndent: Statement =
      statement match
        case s@Statement.`case`(lambda) => s.copy(lambda = lambda.addIndent)
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
        case Statement.`case`(tpe)                                              => tpe.getIndent
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

  extension [A] (term: `Type.Var: x`[A])
    def addIndent: `Type.Var: x`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [F[_]] (term: `Type.Var: x_x`[F])
    def addIndent: `Type.Var: x_x`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [F[_[_]]] (term: `Type.Var: lx_xl_x`[F])
    def addIndent: `Type.Var: lx_xl_x`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in  

  extension [F[_[_], _]] (term: `Type.Var: lx_xl_x_x`[F])
    def addIndent: `Type.Var: lx_xl_x_x`[F] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [A] (term: `Type: x`[A])
    def addIndent: `Type: x`[A] =
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
        case t@`Type.Var: x`(in, nme, impl) => t.copy(in = in + 1)
        case t@`Type.Bot: x`(in) => t.copy(in = in + 1)

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
        case `Type.Var: x`(in, nme, impl) => in
        case `Type.Bot: x`(in) => in

  extension [F[_]] (term: `Type: x_x`[F])
    def addIndent: `Type: x_x`[F] =
      term match
        case t@`Type.Lam: x_x`(in, domain, codomain) => t.copy(in = in + 1)
        case t@`Type.Var: x_x`(in, nme, impl, c) => t.copy(in = in + 1)
        // case t@`Type.Var2[_]`(in, _, _, _, _) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Lam: x_x`(in, domain, codomain) => in
        case `Type.Var: x_x`(in, nme, impl, c) => in
        // case `Type.Var2[_]`(in, _, _, _, _) => in
      
  extension [F[_, _]] (term: `Type: x_x_x`[F])
    def addIndent: `Type: x_x_x`[F] =
      term match
        case t@`Type.Var: x_x_x`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var: x_x_x`(in, nme, impl) => in
        
  extension [F[_[_], _]] (term: `Type: lx_xl_x_x`[F])
    def addIndent: `Type: lx_xl_x_x`[F] =
      term match
        case t@`Type.Var: lx_xl_x_x`(in, nme, impl) => t.copy(in = in + 1)
        case t@`Type.Lam: lx_xl_x_x`(in, _, _, _) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var: lx_xl_x_x`(in, nme, impl) => in
        case `Type.Lam: lx_xl_x_x`(in, _, _, _) => in
      
  extension [F[_, _, _]] (term: `Type.x_x_x_x`[F])
    def addIndent: `Type.x_x_x_x`[F] =
      term match
        case t@`Type.Var: x_x_x_x`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var: x_x_x_x`(in, nme, impl) => in 

  extension [F[_[_], _, _]] (term: `Type.lx_xl_x_x_x`[F])
    def addIndent: `Type.lx_xl_x_x_x`[F] =
      term match
        case t@`Type.Var: lx_xl_x_x_x`(in, nme, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Type.Var: lx_xl_x_x_x`(in, nme, impl) => in

  extension [A] (term: `Value.Var`[A])
    def addIndent: `Value.Var`[A] =
      term match
        case t@`Value.Var.Unbound.Data`(in, nme, tpe) => t.copy(in = in + 1)
        case t@`Value.Var.Bound.Data`(in, nme, tpe, impl) => t.copy(in = in + 1)
    def getIndent: Int =
      term match
        case `Value.Var.Unbound.Data`(in, nme, tpe) => in
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => in

  extension [A] (term: `Value.Var.Unbound.Data`[A])
    def addIndent: `Value.Var.Unbound.Data`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in

  extension [A] (term: `Value.Var.Bound.Data`[A])
    def addIndent: `Value.Var.Bound.Data`[A] =
      term.copy(in = term.in + 1)
    def getIndent: Int =
      term.in
      
  extension [A] (term: `Value: x`[A])
    def addIndent: `Value: x`[A] =
      term match
        case v@`Value.App.1: x`(in, fun, arg, tpe) => v.copy(in = in + 1)
        case v@`Value.App.2: x`(in, fun, arg, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.App.3: x`(in, fun, arg, arg2, arg3, tpe) => v.copy(in = in + 1)
        case `Value.App.Vargs: x`(in, fun, tpe, vargs*) => `Value.App.Vargs: x`(in + 1, fun, tpe, vargsxl
        case v@`Value.AppDot.0: x`(in, fun, arg1, tpe) => v.copy(in = in + 1)
        case v@`Value.AppDot.1: x`(in, fun, arg1, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.AppDotless: x`(in, fun, arg1, arg2, tpe) => v.copy(in = in + 1)
        case v@`Value.AppForComp: x_x x`(in, gens, ret, tpe) => v.copy(in = in + 1, gens.map(s => s.addIndent))
        case `Value.App.Match`(in, value, tpe, l) => `Value.App.Match`(in + 1, value, tpe, l)
        case v@`Value.App.0: x_x`(in, fun, targ, tpe) => v.copy(in = in + 1)
        case v@`Value.App.0: lx_xl_x_x`(in, fun, targf, targa, tpe) => v.copy(in = in + 1)
        case v@`Value.App.0: lx_xl_x_x`(in, fun, targf, targa, targb, tpe) => v.copy(in = in + 1)
        case v@`Value.Lit.Boolean: x`(in, tpe, b) => v.copy(in = in + 1)
        case v@`Value.Lit.Int: x`(in, tpe, i) => v.copy(in = in + 1)
        case v@`Value.Lit.String: x`(in, tpe, s) => v.copy(in = in + 1)
        case v@`Value.Lit.Unit: x`(in, tpe, u) => v.copy(in = in + 1)
        case v@`Value.Lam.1: x_x_x x x`(in, a, b, tpe) => v.copy(in = in + 1)
        case v@`Value.Lam.2: x_x_x_x x * x`(in, a1, a2, r, tpe) => v.copy(in = in + 1)
        case v@`Value.Lam.3: x_x_x_x_x x * x x`(in, a1, a2, a3, r, tpe) => v.copy(in = in + 1)
        case v@`Value.Var.Unbound.Data`(in, nme, tpe) => v.copy(in = in + 1)
        case v@`Value.Var.Bound.Data`(in, nme, tpe, impl) => v.copy(in = in + 1)
      
    def getIndent: Int =
      term match
        case `Value.App.1: x`(in, fun, arg, tpe) => in
        case `Value.App.2: x`(in, fun, arg, arg2, tpe) => in
        case `Value.App.3: x`(in, fun, arg, arg2, arg3, tpe) => in
        case `Value.App.Vargs: x`(in, fun, tpe, vargsxl => in
        case `Value.AppDot.0: x`(in, fun, arg1, tpe) => in
        case `Value.AppDot.1: x`(in, fun, arg1, arg2, tpe) => in
        case `Value.AppDotless: x`(in, fun, arg1, arg2, tpe) => in
        case `Value.AppForComp: x_x x`(in, gens, ret, tpe) => in
        case `Value.App.Match`(in, value, tpe, cases) => in
        case `Value.App.0: x_x`(in, fun, targ, tpe) => in
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, tpe) => in
        case `Value.App.0: lx_xl_x_x`(in, fun, targf, targa, targb, tpe) => in
        case `Value.Lit.Boolean: x`(in, tpe, b) => in
        case `Value.Lit.Int: x`(in, tpe, i) => in
        case `Value.Lit.String: x`(in, tpe, s) => in
        case `Value.Lit.Unit: x`(in, tpe, u) => in
        case `Value.Lam.1: x_x_x x x`(in, a, b, tpe) => in
        case `Value.Lam.2: x_x_x_x x * x`(in, a1, a2, r, tpe) => in
        case `Value.Lam.3: x_x_x_x_x x * x x`(in, a1, a2, a3, r, tpe) => in
        case `Value.Var.Unbound.Data`(in, nme, tpe) => in
        case `Value.Var.Bound.Data`(in, nme, tpe, impl) => in
      
  extension [F[_]] (term: `Value.x_x`[F])
    def addIndent: `Value.x_x`[F] =
      term match
        case v@`Value.Var0[_]`(in, nme, tpe, impl) => v.copy(in = in +1)
        case v@`Value.Var1[_]`(in, nme, tpe, impl) => v.copy(in = in +1)
        case v@`Value.Lam.1: x_x`(in, fa, tpe) => v.copy(in = in +1)
    def getIndent: Int =
      term match
        case `Value.Var0[_]`(in, nme, tpe, impl) => in
        case `Value.Var1[_]`(in, nme, tpe, impl) => in
        case `Value.Lam.1: x_x`(in, fa, tpe) => in