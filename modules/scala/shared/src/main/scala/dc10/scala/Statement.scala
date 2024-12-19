package dc10.scala

sealed trait Statement
object Statement:
  
  case class `case class`[T](tpe: Type.Var[T], fields: List[Statement], body: List[Statement]) extends Statement
  case class `extension`[T](field: Value.VarA[T], body: List[Statement]) extends Statement
  object `def`:
    case class `0`[A](value: Value.`Var`[A]) extends Statement
    object `0`:
      case class `[_]`[T, A](tparam: `Type.*`[A], impl: Option[`Value.*`[T]], value: Value.`Var`[T]) extends Statement
      case class `[_[_]]`[F[_], A](tparam: `Type.*->*`[F], impl: Option[`Value.*`[A]], value: Value.`Var`[A]) extends Statement
      case class `[_[_], _]`[F[_], A, T](tparamf: `Type.*->*`[F], tparama: `Type.*`[A], impl: Option[`Value.*`[T]], value: Value.`Var`[T]) extends Statement
    case class `1`[A, B](arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[B]], value: Value.`Var`[A => B]) extends Statement
    object `1`:
      case class `[_]`[T, A, B](tparam: `Type.*`[A], arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[T]], value: Value.`Var`[T]) extends Statement
      case class `[_[_], _]`[F[_], T, A, B](tparamf: `Type.*->*`[F], tparama: `Type.*`[A], arg: `Value.*`[A], ret: `Type.*`[B], impl: Option[`Value.*`[T]], value: Value.`Var`[T]) extends Statement
  case class `field`[T](value: Value.`Var`[T] ) extends Statement
  case class `generator`[F[_], A](value: Value.VarC[F[A]]) extends Statement
  case class `object`[T](value: Value.VarA[T], parent: Option[`Type.*`[T]], body: List[Statement]) extends Statement
  case class `package`(nme: Option[String], contents: List[Statement]) extends Statement
  case class `trait`[T](tpe: Type.Var[T], parent: Option[Type], body: List[Statement]) extends Statement
  object `trait`:
    case class `[_]`[T[_], A](tpe: Type.`Var[_]`[T], tparam: `Type.*`[A], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_]]`[T[_[_]], F[_]](tpe: Type.`Var[_[_]]`[T], tparam: `Type.*->*`[F], parent: Option[Type], body: List[Statement]) extends Statement
    case class `[_[_], _]`[T[_[_], _], F[_], A](tpe: Type.`Var[_[_], _]`[T], tparamF: `Type.*->*`[F], tparamA: `Type.*`[A], parent: Option[Type], body: List[Statement]) extends Statement
  case class `type`[T](tpe: Type.`Var`[T]) extends Statement
  object `type`:
    case class `[_]`[F[_], A](tparam: `Type.*`[A], tpe: Type.`Var[_]`[F]) extends Statement
    case class `[_]=>>`[F[_]](tpe: Type.`Var[_]`[F]) extends Statement
    case class `[_[_]]`[F[_[_]], G[_]](tparam: `Type.*->*`[G], tpe: Type.`Var[_[_]]`[F]) extends Statement
    case class `[_[_], _]`[F[_[_], _], G[_], A](tparamF: `Type.*->*`[G], tparamA: `Type.*`[A], tpe: Type.`Var[_[_], _]`[F]) extends Statement
  case class `val`[T](value: Value.`Var`[T]) extends Statement

  extension (s: Statement)
    def addIndent: Statement =
      s match
        case d@`case class`(t, f, b)                               => `case class`(t.addIndent, f.map(v => v.addIndent), b.map(s => s.addIndent))
        case d@`extension`(f, b)                                   => `extension`(f.copy(indent = f.indent + 1), b.map(s => s.addIndent))
        case d@`object`(t, p, b)                                   => d.copy(value = d.value.copy(indent = d.value.indent + 1), body = d.body.map(s => s.addIndent))
        case d@`package`(nme, contents)                            => `package`(nme, contents)
        case d@`trait`(t, p, b)                                    => d.copy(tpe = t.addIndent, body = b.map(s => s.addIndent))
        case d@`trait`.`[_]`(t, a, p, b)                           => d.copy(tpe = t.addIndent, body = b.map(s => s.addIndent))
        case d@`trait`.`[_[_]]`(t, f, p, b)                        => d.copy(tpe = t.addIndent, body = b.map(s => s.addIndent))
        case d@`trait`.`[_[_], _]`(t, f, a, p, b)                  => d.copy(tpe = t.addIndent, body = b.map(s => s.addIndent))
        case d@`type`(tpe)                                         => `type`(tpe.addIndent)
        case d@`type`.`[_]=>>`(tpe)                                => `type`.`[_]=>>`(tpe.addIndent)
        case d@`type`.`[_]`(a, t)                                  => `type`.`[_]`(d.tparam, d.tpe.addIndent)
        case d@`type`.`[_[_]]`(a, t)                               => `type`.`[_[_]]`(d.tparam, d.tpe.addIndent)
        case d@`type`.`[_[_], _]`(f, a, t)                         => `type`.`[_[_], _]`(f, a, d.tpe.addIndent)
        case d@`def`.`0`(value)                                    => `def`.`0`(value.addIndent)
        case d@`def`.`0`.`[_]`(tparam, ret, value)                 => `def`.`0`.`[_]`(tparam, ret, value.addIndent)
        case d@`def`.`0`.`[_[_]]`(tparam, ret, value)              => `def`.`0`.`[_[_]]`(tparam, ret, value.addIndent)
        case d@`def`.`0`.`[_[_], _]`(tparamf, tparama, ret, value) => `def`.`0`.`[_[_], _]`(tparamf, tparama, ret, value.addIndent)
        case d@`def`.`1`(arg, ret, impl, value)                    => `def`.`1`(arg, ret, impl.map(v => v.addIndent), value.addIndent)
        case d@`def`.`1`.`[_]`(tparam, arg, ret, impl, value)      => `def`.`1`.`[_]`(tparam, arg, ret, impl.map(v => v.addIndent), value.addIndent)
        case d@`def`.`1`.`[_[_], _]`(f, a, arg, ret, impl, value)  => `def`.`1`.`[_[_], _]`(f, a, arg, ret, impl.map(v => v.addIndent), value.addIndent)
        case d@`field`(v)                                          => d.copy(value = v.addIndent)
        case d@`generator`(v)                                      => `generator`(v.copy(indent = v.indent + 1))
        case d@`val`(v)                                            => d.copy(value = v.addIndent)
    def getIndent: Int =
      s match
        case `case class`(tpe, fields, body)                          => tpe.indent
        case `extension`(field, body)                                 => field.indent
        case `def`.`0`(value)                                         => value.getIndent
        case `def`.`0`.`[_]`(tparam, impl, value)                     => value.getIndent
        case `def`.`0`.`[_[_]]`(tparam, impl, value)                  => value.getIndent
        case `def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value)     => value.getIndent
        case `def`.`1`(arg, impl, ret, value)                         => value.getIndent
        case `def`.`1`.`[_]`(tparam, arg, impl, ret, value)           => value.getIndent
        case `def`.`1`.`[_[_], _]`(f, a, arg, impl, ret, value)       => value.getIndent
        case `field`(value)                                           => value.getIndent
        case `generator`(value)                                       => value.indent
        case `object`(value, parent, body)                            => value.indent
        case `package`(nme, contents)                                 => 0
        case `trait`(tpe, parent, body)                               => tpe.indent
        case `trait`.`[_]`(tpe, tparam, parent, body)                 => tpe.indent
        case `trait`.`[_[_]]`(tpe, tparam, parent, body)              => tpe.indent
        case `trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => tpe.indent
        case `type`(tpe)                                              => tpe.indent
        case `type`.`[_]`(tparam, tpe)                                => tpe.indent
        case `type`.`[_]=>>`(tpe)                                     => tpe.indent
        case `type`.`[_[_]]`(tparam, tpe)                             => tpe.indent
        case `type`.`[_[_], _]`(tparamF, tparamA, tpe)                => tpe.indent
        case `val`(value)                                             => value.getIndent
    def getValue[T]: Either[List[Error], `Value.*`[T]] =
      s match
        case `case class`(tpe, fields, body) => Left(List(Error("Not a value level definition"))) 
        case `extension`(field, body) => Left(List(Error("Not a value level definition")))
        case `def`.`0`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`0`.`[_]`(tparam, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`0`.`[_[_]]`(tparam, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`0`.`[_[_], _]`(tparamf, tparama, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`1`(arg, ret, impl, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`1`.`[_]`(tparam, arg, impl, ret, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `def`.`1`.`[_[_], _]`(tparamf, tparama, arg, impl, ret, value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `field`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `generator`(value) => Right(value.asInstanceOf[`Value.*`[T]])
        case `object`(value, parent, body) => Right(value.asInstanceOf[`Value.*`[T]])
        case `package`(nme, contents) => Left(List(Error("Not a value level definition")))
        case `trait`(tpe, parent, body) => Left(List(Error("Not a value level definition")))
        case `trait`.`[_]`(tpe, tparam, parent, body) => Left(List(Error("Not a value level definition")))
        case `trait`.`[_[_]]`(tpe, tparam, parent, body) => Left(List(Error("Not a value level definition")))
        case `trait`.`[_[_], _]`(tpe, tparamF, tparamA, parent, body) => Left(List(Error("Not a value level definition")))
        case `type`(tpe) => Left(List(Error("Not a value level definition")))
        case `type`.`[_]`(tparam, tpe) => Left(List(Error("Not a value level definition")))
        case `type`.`[_]=>>`(tpe) => Left(List(Error("Not a value level definition")))
        case `type`.`[_[_]]`(tparam, tpe) => Left(List(Error("Not a value level definition")))
        case `type`.`[_[_], _]`(tparamF, tparamA, tpe) => Left(List(Error("Not a value level definition")))
        case `val`(value) => Right(value.asInstanceOf[`Value.*`[T]])
      