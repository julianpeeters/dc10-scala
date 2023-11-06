package dc10.scalaq

  import dc10.scala.Symbol.Term

  extension [T, Z] (t: Term.TypeLevel[T, Z])
    def manageDep[ZZ](f: Z => ZZ): Term.TypeLevel[T, ZZ] =
      t match
        case Term.TypeLevel.App.App1(qnt, tfun, targ, dep) =>  Term.TypeLevel.App.App1(qnt, tfun, targ, f(dep)) 
        case Term.TypeLevel.App.App2(qnt, tfun, ta, tb, dep) => Term.TypeLevel.App.App2(qnt, tfun.manageDep(f), ta.manageDep(f), tb.manageDep(f), f(dep))
        case Term.TypeLevel.App.App3(qnt, tfun, ta1, ta2, tb, dep) => Term.TypeLevel.App.App3(qnt, tfun.manageDep(f), ta1.manageDep(f), ta2.manageDep(f), tb.manageDep(f), f(dep))
        case Term.TypeLevel.Lam.Function1Type(qnt, dep) => Term.TypeLevel.Lam.Function1Type(qnt, f(dep)).asInstanceOf[Term.TypeLevel[T, ZZ]]
        case Term.TypeLevel.Lam.Function2Type(qnt, dep) => Term.TypeLevel.Lam.Function2Type(qnt, f(dep)).asInstanceOf[Term.TypeLevel[T, ZZ]]
        case Term.TypeLevel.Var.BooleanType(qnt, dep) => Term.TypeLevel.Var.BooleanType(qnt, f(dep))
        case Term.TypeLevel.Var.IntType(qnt, dep) => Term.TypeLevel.Var.IntType(qnt, f(dep))
        case Term.TypeLevel.Var.StringType(qnt, dep) => Term.TypeLevel.Var.StringType(qnt, f(dep))
        case Term.TypeLevel.Var.ListType(qnt, dep) => Term.TypeLevel.Var.ListType(qnt, f(dep))
        case Term.TypeLevel.Var.OptionType(qnt, dep) => Term.TypeLevel.Var.OptionType(qnt, f(dep))
        case Term.TypeLevel.Var.SomeType(qnt, dep) => Term.TypeLevel.Var.SomeType(qnt, f(dep))
        case Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl, dep) => Term.TypeLevel.Var.UserDefinedType(qnt, nme, impl.map(i => i.manageDep(f)), f(dep))

  extension [T, Z] (v: Term.ValueLevel[T, Z])
    private def manageDep[ZZ](f: Z => ZZ): Term.ValueLevel[T, ZZ] =
      v match
        case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => Term.ValueLevel.App.App1(qnt, fun.manageDep(f), arg.manageDep(f), tpe.manageDep(f))
        case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => Term.ValueLevel.App.AppCtor1(qnt, tpe.manageDep(f), arg.manageDep(f))
        case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe.manageDep(f))
        case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => ???
        case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => Term.ValueLevel.App.Dot1(qnt, fun.manageDep(f), arg1.manageDep(f), arg2.manageDep(f), tpe.manageDep(f))
        case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => Term.ValueLevel.App.Dotless(qnt, fun.manageDep(f), arg1.manageDep(f), arg2.manageDep(f), tpe.manageDep(f))
        case Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => ???
        case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, tpe) => ???
        case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => Term.ValueLevel.Var.BooleanLiteral(qnt, tpe.manageDep(f), b)
        case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => Term.ValueLevel.Var.IntLiteral(qnt, tpe.manageDep(f), i)
        case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => Term.ValueLevel.Var.StringLiteral(qnt, tpe.manageDep(f), s)
        case Term.ValueLevel.Var.ListCtor(qnt, tpe) => Term.ValueLevel.Var.ListCtor(qnt, tpe.manageDep(f))
        case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => Term.ValueLevel.Var.OptionCtor(qnt, tpe.manageDep(f))
        case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => Term.ValueLevel.Var.SomeCtor(qnt, tpe.manageDep(f))
        case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe.manageDep(f), impl.map(i => i.manageDep(f)))

    private def findImpl: Option[Term.ValueLevel[T, Z]] =
      v match
        case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => Some(v)
        case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => Some(v)
        case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => Some(v)
        case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => Some(v)
        case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => Some(v)
        case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => Some(v)
        case Term.ValueLevel.Lam.Lam1(qnt, a, b, t) => Some(v)
        case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, t) => Some(v)
        case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => Some(v)
        case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => Some(v)
        case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => Some(v)
        case Term.ValueLevel.Var.ListCtor(qnt, tpe) => Some(v)
        case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => Some(v)
        case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => Some(v)
        case u@Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => impl.fold(None)(i => i.findImpl)

    private def findVargs[U, A]: Option[Seq[Term.ValueLevel[U, A]]] =
      v.findImpl.fold(None)(i => i match
        case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => None 
        case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => None
        case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => None
        case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => Some(vargs.asInstanceOf[Seq[Term.ValueLevel[U, A]]])
        case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => None
        case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => None
        case Term.ValueLevel.Lam.Lam1(qnt, a, b, t) => None
        case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, b, t) => None
        case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => None
        case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => None
        case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => None
        case Term.ValueLevel.Var.ListCtor(qnt, tpe) => None
        case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => None
        case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => None
        case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => None
      )