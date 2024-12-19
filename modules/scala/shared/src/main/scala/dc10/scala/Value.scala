package dc10.scala

sealed trait Value
sealed trait `Value.*`[T] extends Value
sealed trait `Value.*->*`[T[_]] extends Value
sealed trait `Value.(*->*)->*`[T[_[_]]] extends Value
sealed trait `Value.*->*->*`[T[_, _]] extends Value
sealed trait `Value.(*->*)->*->*`[T[_[_], _]] extends Value

object Value:

  case class App1[A, B](indent: Int, fun: `Value.*`[A => B], arg: `Value.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]
  case class App2[A, B, C](indent: Int, fun: `Value.*`[(A, B) => C], arg: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
  case class App3[A, B, C, D](indent: Int, fun: `Value.*`[(A, B, C) => D], arg: `Value.*`[A], arg2: `Value.*`[B], arg3: `Value.*`[C], tpe: `Type.*`[D]) extends `Value.*`[D]
  case class AppPure[G[_], A](indent: Int, fun: `Value.*`[G[A]], arg: `Value.*`[A], tpe: `Type.*`[G[A]]) extends `Value.*`[G[A]]
  case class AppVargs[A, B](indent: Int, fun: `Value.*`[List[A] => B], tpe: `Type.*`[B], vargs: `Value.*`[A]*) extends `Value.*`[B]
  case class AppDot0[A, B, C](indent: Int, fun: `Value.*`[C], arg1: `Value.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]
  case class AppDot1[A, B, C, D](indent: Int, fun: `Value.*`[D], arg1: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
  case class AppDotless[A, B, C, D](indent: Int, fun: `Value.*`[D], arg1: `Value.*`[A], arg2: `Value.*`[B], tpe: `Type.*`[C]) extends `Value.*`[C]
  case class AppForComp[G[_], A](indent: Int, gens: List[Statement], ret: `Value.*`[A], tpe: `Type.*`[G[A]]) extends `Value.*`[G[A]]
  case class AppType[G[_], A, B](indent: Int, fun: `Value.*->*`[G], targ: `Type.*`[A], tpe: `Type.*`[B]) extends `Value.*`[B]
  
  case class LitBoolean(indent: Int, tpe: `Type.*`[Boolean], b: Boolean) extends `Value.*`[Boolean]
  case class LitInt(indent: Int, tpe: `Type.*`[Int], i: Int) extends `Value.*`[Int]
  case class LitString(indent: Int, tpe: `Type.*`[String], s: String) extends `Value.*`[String]
  case class LitUnit(indent: Int, tpe: `Type.*`[Unit], u: Unit) extends `Value.*`[Unit]
  
  case class Lam1[A, B](indent: Int, a: `Value.*`[A], b: `Value.*`[B], tpe: `Type.*`[A => B]) extends `Value.*`[A => B]
  case class Lam2[A, B, C](indent: Int, a1: `Value.*`[A], a2: `Value.*`[B], r: `Value.*`[C], tpe: `Type.*`[(A, B) => C]) extends `Value.*`[(A, B) => C]
  case class Lam3[A, B, C, D](indent: Int, a1: `Value.*`[A], a2: `Value.*`[B], a3: `Value.*`[C], r: `Value.*`[D], tpe: `Type.*`[(A, B, C) => D]) extends `Value.*`[(A, B, C) => D]
 
  sealed trait Var[T] extends `Value.*`[T]
  sealed trait VarAbstract[T] extends Var[T]
  sealed trait VarConcrete[T] extends Var[T]
  case class VarA[T](indent: Int, nme: String, tpe: `Type.*`[T]) extends VarAbstract[T]
  case class VarC[T](indent: Int, nme: String, tpe: `Type.*`[T], impl: `Value.*`[T]) extends VarConcrete[T]
  // case class `Var[_]`[T[_]](indent: Int, nme: String, tpe: `Type.*->*`[T], impl: Option[`V*->*`[T]]) extends `V*->*`[T]
  case class `Var0[_]`[T[_]](indent: Int, nme: String, tpe: `Type.*->*`[[A] =>> T[A]], impl: Option[`Value.*->*`[[A] =>> T[A]]]) extends `Value.*->*`[[A] =>> T[A]]
  case class `Var1[_]`[F[_], G[_]](indent: Int, nme: String, tpe: `Type.*->*`[[A] =>> F[A] => G[A]], impl: Option[`Value.*->*`[[A] =>> F[A] => G[A]]]) extends `Value.*->*`[[A] =>> F[A] => G[A]]
  // case class `Var[_==>]`[T[_]](indent: Int, nme: String, tpe: `Type.*->*`[[A] =>> T[A]], impl: Option[`V*->*`[[A] =>> T[A]]]) extends `V*->*`[[A] =>> T[A]]
  case class `Var[_[_]]`[T[_[_]]](indent: Int, nme: String, tpe: `Type.(*->*)->*`[T], impl: Option[`Value.(*->*)->*`[T]]) extends `Value.(*->*)->*`[T]
  case class `Var[_, _]`[T[_, _]](indent: Int, nme: String, tpe: `Type.*->*->*`[T], impl: Option[`Value.*->*->*`[T]]) extends `Value.*->*->*`[T]
  case class `Var[_[_], _]`[T[_[_], _]](indent: Int, nme: String, tpe: `Type.(*->*)->*->*`[T], impl: Option[`Value.(*->*)->*->*`[T]]) extends `Value.(*->*)->*->*`[T]

  extension [T] (v: `Value.*`[T])
    def addIndent: `Value.*`[T] =
      v match
        case d@App1(indent, fun, arg, tpe) => d.copy(indent = indent + 1)
        case d@App2(indent, fun, arg, arg2, tpe) => d.copy(indent = indent + 1)
        case d@App3(indent, fun, arg, arg2, arg3, tpe) => d.copy(indent = indent + 1)
        case d@AppPure(indent, fun, arg, tpe) => d.copy(indent = indent + 1)
        case d@AppVargs(indent, fun, tpe, vargs*) => AppVargs(indent + 1, fun, tpe, vargs*)
        case d@AppDot0(indent, fun, arg1, tpe) => d.copy(indent = indent + 1)
        case d@AppDot1(indent, fun, arg1, arg2, tpe) => d.copy(indent = indent + 1)
        case d@AppDotless(indent, fun, arg1, arg2, tpe) => d.copy(indent = indent + 1)
        case d@AppForComp(indent, gens, ret, tpe) => d.copy(indent = indent + 1, gens.map(s => s.addIndent))
        case d@AppType(indent, fun, targ, tpe) => d.copy(indent = indent + 1)
        case d@LitBoolean(indent, tpe, b) => d.copy(indent = indent + 1)
        case d@LitInt(indent, tpe, i) => d.copy(indent = indent + 1)
        case d@LitString(indent, tpe, s) => d.copy(indent = indent + 1)
        case d@LitUnit(indent, tpe, u) => d.copy(indent = indent + 1)
        case d@Lam1(indent, a, b, tpe) => d.copy(indent = indent + 1)
        case d@Lam2(indent, a1, a2, c, tpe) => d.copy(indent = indent + 1)
        case d@Lam3(indent, a1, a2, a3, c, tpe) => d.copy(indent = indent + 1)
        case d@VarA(indent, nme, tpe) => d.copy(indent = indent + 1)
        case d@VarC(indent, nme, tpe, impl) => d.copy(indent = indent + 1, impl = impl.addIndent)
    def assign(rhs: `Value.*`[T]): Either[List[Error], Value.VarC[T]] =
      v match
        case App1(indent, fun, arg, tpe) => Left(List(Error(s"Value is not assignable ${v}"))) 
        case App2(indent, fun, arg, arg2, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case App3(indent, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppPure(indent, fun, arg, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppVargs(indent, fun, tpe, vargs*) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppDot0(indent, fun, arg1, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppDot1(indent, fun, arg1, arg2, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppDotless(indent, fun, arg1, arg2, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppForComp(indent, gens, ret, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case AppType(indent, fun, targ, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case LitBoolean(indent, tpe, b) => Left(List(Error(s"Value is not assignable ${v}")))
        case LitInt(indent, tpe, i) => Left(List(Error(s"Value is not assignable ${v}")))
        case LitString(indent, tpe, s) => Left(List(Error(s"Value is not assignable ${v}")))
        case LitUnit(indent, tpe, u) => Left(List(Error(s"Value is not assignable ${v}")))
        case Lam1(indent, a, b, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case Lam2(indent, a1, a2, c, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case Lam3(indent, a1, a2, a3, r, tpe) => Left(List(Error(s"Value is not assignable ${v}")))
        case VarA(indent, nme, tpe) => Right(`VarC`(indent, nme, tpe, rhs.addIndent))
        case VarC(indent, nme, tpe, impl) => Left(List(Error(s"Value is already assigned ${v}")))
    def getIndent: Int =
      v match
        case App1(indent, fun, arg, tpe) => indent
        case App2(indent, fun, arg, arg2, tpe) => indent
        case App3(indent, fun, arg, arg2, arg3, tpe) => indent
        case AppPure(indent, fun, arg, tpe) => indent
        case AppVargs(indent, fun, tpe, vargs*) => indent
        case AppDot0(indent, fun, arg1, tpe) => indent
        case AppDot1(indent, fun, arg1, arg2, tpe) => indent
        case AppDotless(indent, fun, arg1, arg2, tpe) => indent
        case AppForComp(indent, gens, ret, tpe) => indent
        case AppType(indent, fun, targ, tpe) => indent
        case LitBoolean(indent, tpe, b) => indent
        case LitInt(indent, tpe, i) => indent
        case LitString(indent, tpe, s) => indent
        case LitUnit(indent, tpe, u) => indent
        case Lam1(indent, a, b, tpe) => indent
        case Lam2(indent, a1, a2, c, tpe) => indent
        case Lam3(indent, a1, a2, a3, r, tpe) => indent
        case VarA(indent, nme, tpe) => indent
        case VarC(indent, nme, tpe, impl) => indent
    def tpe: `Type.*`[T] =
      v match
        case App1(indent, fun, arg, tpe) => tpe
        case App2(indent, fun, arg1, arg2, tpe) => tpe 
        case App3(indent, fun, arg1, arg2, arg3, tpe) => tpe 
        case AppPure(indent, fun, arg, tpe) => tpe
        case AppVargs(indent, fun, tpe, vargs*) => tpe
        case AppDot0(indent, fun, arg1, tpe) => tpe
        case AppDot1(indent, fun, arg1, arg2, tpe) => tpe
        case AppDotless(indent, fun, arg1, arg2, tpe) => tpe
        case AppForComp(indent, l, r, tpe) => tpe
        case AppType(indent, fun, targ, tpe) => tpe
        case Lam1(indent, a, b, tpe) => tpe
        case Lam2(indent, a1, a2, c, tpe) => tpe
        case Lam3(indent, a1, a2, a3, r, tpe) => tpe
        case LitBoolean(indent, tpe, b) => tpe
        case LitInt(indent, tpe, i) => tpe
        case LitString(indent, tpe, s) => tpe
        case LitUnit(indent, tpe, u) => tpe
        case VarA(indent, nme, tpe) => tpe  
        case VarC(indent, nme, tpe, impl) => tpe  
    def findImpl: Option[`Value.*`[T]] =
      v match
        case App1(indent, fun, arg, tpe) => Some(v)
        case App2(indent, fun, arg1, arg2, tpe) => Some(v) 
        case App3(indent, fun, arg1, arg2, arg3, tpe) => Some(v) 
        case AppPure(indent, fun, arg, tpe) => Some(v)
        case AppVargs(indent, fun, tpe, vargs*) => Some(v)
        case AppDot0(indent, fun, arg1, tpe) => Some(v)
        case AppDot1(indent, fun, arg1, arg2, tpe) => Some(v)
        case AppDotless(indent, fun, arg1, arg2, tpe) => Some(v)
        case AppForComp(indent, l, r, t) => Some(v)
        case AppType(indent, fun, targ, tpe) => Some(v)
        case Lam1(indent, a, b, t) => Some(v)
        case Lam2(indent, a1, a2, b, t) => Some(v)
        case Lam3(indent, a1, a2, a3, b, t) => Some(v)
        case LitBoolean(indent, tpe, b) => Some(v)
        case LitInt(indent, tpe, i) => Some(v)
        case LitString(indent, tpe, s) => Some(v)
        case LitUnit(indent, tpe, s) => Some(v)
        case VarA(indent, nme, tpe) => None
        case VarC(indent, nme, tpe, impl) => impl.findImpl

  extension [T] (v: `Var`[T])
    def addIndent: `Var`[T] =
      v match
        case d@VarA(indent, nme, tpe) => d.copy(indent = indent + 1)
        case d@VarC(indent, nme, tpe, impl) => d.copy(indent = indent + 1, impl = impl.addIndent)
    def getName: String =
      v match
        case VarA(indent, nme, tpe) => nme
        case VarC(indent, nme, tpe, impl) => nme

  extension [G[_], A] (v: `Value.*`[G[A]])
    def unapplyRightmost: Either[List[Error], `Value.*`[A]] =
      v match
        case App1(indent, fun, arg, tpe) => Right(arg.asInstanceOf[`Value.*`[A]])
        case App2(indent, fun, arg, arg2, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case App3(indent, fun, arg, arg2, arg3, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case AppPure(indent, fun, arg, tpe) => Right(arg)
        case AppVargs(indent, fun, tpe, vargs*) => Left(List(Error(s"not a functor value ${v}")))
        case AppDot0(indent, fun, arg1, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case AppDot1(indent, fun, arg1, arg2, tpe) => Right(arg2.asInstanceOf[`Value.*`[A]])
        case AppDotless(indent, fun, arg1, arg2, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case AppForComp(indent, gens, ret, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case AppType(indent, fun, targ, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case LitBoolean(indent, tpe, b) => Left(List(Error(s"not a functor value ${v}")))
        case LitInt(indent, tpe, i) => Left(List(Error(s"not a functor value ${v}")))
        case LitString(indent, tpe, s) => Left(List(Error(s"not a functor value ${v}")))
        case LitUnit(indent, tpe, u) => Left(List(Error(s"not a functor value ${v}")))
        case Lam1(indent, a, b, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case Lam2(indent, a1, a2, c, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case Lam3(indent, a1, a2, a3, r, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case VarA(indent, nme, tpe) => Left(List(Error(s"not a functor value ${v}")))
        case VarC(indent, nme, tpe, impl) => Left(List(Error(s"not a functor value ${v}")))
      
  extension [A, B] (v: `Value.*`[A => B])
    def returnType: Either[List[Error], `Type.*`[B]] =
      v.tpe.returnType[B]
      
  extension [T[_]] (v: `Value.*->*`[T])
    def indent: Int =
      v match
        case `Var0[_]`(indent, nme, tpe, impl) => indent
        case `Var1[_]`(indent, nme, tpe, impl) => indent
    def nme: String =
      v match
        case `Var0[_]`(indent, nme, tpe, impl) => nme
        case `Var1[_]`(_, nme, _, _) => nme
    def tpe: `Type.*->*`[T] =
      v match
        case `Var0[_]`(indent, nme, tpe, impl) => tpe
        case `Var1[_]`(_, _, tpe, _) => tpe

  extension [T[_]] (v: `Value.*->*`[[A] =>> A => T[A]])
    @scala.annotation.targetName("apply T [A] =>> A => T[A]")
    def apply[A](targ: `Type.*`[A]): Either[List[Error], `Value.*`[A => T[A]]] =
      v.tpe(targ).map(r => Value.AppType(v.indent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> A => T[A]")
    def apply[A](a: `Value.*`[A]): Either[List[Error], `Value.*`[T[A]]] =
      for
        c <- v(a.tpe)
        t <- v.tpe(a.tpe)
        r <- t.unapplyRightmost
      yield Value.App1(v.indent, c, a, r)

      
  extension [T[_]] (v: `Value.*->*`[[A] =>> List[A] => T[A]])
    @scala.annotation.targetName("apply T [A] =>> List[A] => T[A]")
    def apply[A](targ: `Type.*`[A]): Either[List[Error], `Value.*`[List[A] => T[A]]] =
      v.tpe.applyVargs(targ).map(r => Value.AppType(v.indent, v, targ, r))
    @scala.annotation.targetName("apply V [A] =>> List[A] => T[A]")
    def apply[A](a: `Value.*`[A]*): Either[List[Error], `Value.*`[T[A]]] =
      for
        x <- a.toList.headOption.fold(Right(Type.`Bot`(v.indent)))(a => Right(a.tpe))
        c <- v(x)
        t <- v.tpe.applyVargs(x)
        r <- t.returnType
      yield
        Value.AppVargs[A, T[A]](v.indent, c, r, a*)
            
          
        
      

      
      