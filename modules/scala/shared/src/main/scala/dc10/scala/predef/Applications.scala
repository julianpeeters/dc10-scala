package dc10.scala.predef

import cats.data.StateT
import cats.implicits.given
import dc10.scala.{Error, ErrorF, LibDep, Statement}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_]]`, `Type[_, _]`, `Type[_[_], _]`, `Type[_[_], _, _]`}
import dc10.scala.Statement.ValueExpr.{`Value`}
import dc10.scala.Symbol.Term

trait Applications[F[_]]:

  extension [T[_]] (function: F[`Type[_]`[T]])
    @scala.annotation.targetName("F[A]")
    def apply[A](args: F[`Type`[A]]): F[`Type`[T[A]]]

  // extension [T[_]] (function: F[`Type[_]`[T]])
  //   @scala.annotation.targetName("F[A]2")
  //   def apply[G[_[_], _]](args: F[`Type[_[_], _]`[G]]): F[`Type[_[_], _]`[G]]

  extension [T[_[_]]] (tfunction: F[`Type[_[_]]`[T]])
    @scala.annotation.targetName("F[G]")
    def apply[G[_]](farg: F[`Type[_]`[G]]): F[`Type`[T[G]]]

  extension [T[_,_]] (tfunction: F[`Type[_, _]`[T]])
    @scala.annotation.targetName("F[A, B]")
    def apply[A, B](fta: F[`Type`[A]], ftb: F[`Type`[B]]): F[`Type`[T[A, B]]]

  extension [T[_[_],_]] (tfunction: F[`Type[_[_], _]`[T]])
    @scala.annotation.targetName("F[G, A]")
    def apply[G[_], A](farg: F[`Type[_]`[G]], aarg: F[`Type`[A]]): F[`Type`[T[G, A]]]

  extension [T[_[_],_,_]] (tfunction: F[`Type[_[_], _, _]`[T]])
    @scala.annotation.targetName("F[G, A, B]")
    def apply[G[_], A, B](farg: F[`Type[_]`[G]], aarg: F[`Type`[A]], barg: F[`Type`[B]]): F[`Type`[T[G, A, B]]]

  extension [A, B] (function: F[Value[A => B]])
    @scala.annotation.targetName("A => B")
    def apply(args: F[Value[A]]): F[Value[B]]

  // extension [A, B] (arg1: F[Type[A]])
  //   @scala.annotation.targetName("dot1T")
  //   def DOT(func: F[Value[A => B]])(arg2: F[Value[B]]): F[Value[B]]

  extension [A, B] (arg1: F[Value[A]])
    @scala.annotation.targetName("dot1V_fa")
    def DOT(func: F[Value[A => B]])(arg2: F[Value[B]]): F[Value[B]]

object Applications:

  trait Mixins extends Applications[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    extension [T[_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]])
      @scala.annotation.targetName("F[A]")
      def apply[A](
        args: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[A]]] =
        for
          f <- tfunction
          a <- args
        yield  Type(Term.TypeLevel.App.`App[_]`(f.tpe, a.tpe))


  // extension [T[_]] (function: F[`Type[_]`[T]])
  //   @scala.annotation.targetName("F[A]2")
  //   def apply[G[_[_], _]](args: F[`Type[_[_], _]`[G]]): F[`Type[_[_], _]`[G]]

    // extension [T[_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[T]])
    //   @scala.annotation.targetName("F[A]2")
    //   def apply[G[_[_], _]](
    //     args: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[G]]
    //   ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[G]] =
    //     for
    //       f <- tfunction
    //       a <- args
    //     yield  `Type[_[_], _]`(Term.TypeLevel.App.`App[_[_[_], _]]`(f.tpe, a.tpe))



    extension [T[_[_]]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_]]`[T]])
      @scala.annotation.targetName("F[G]")
      def apply[G[_]](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[G]]] =
        for
          t <- tfunction
          f <- farg
        yield Type(Term.TypeLevel.App.`App[_[_]]`(t.tpe, f.tpe))

        
    extension [T[_,_]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_, _]`[T]])
      @scala.annotation.targetName("F[A, B]")
      def apply[A, B](
        fta: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
        ftb: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[A, B]]] =
        for
          f <- tfunction
          a <- fta
          b <- ftb
        yield Type(Term.TypeLevel.App.`App[_, _]`(f.tpe, a.tpe, b.tpe))

    extension [T[_[_], _]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[T]])
      @scala.annotation.targetName("F[G, A]")
      def apply[G[_], A](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
        aarg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[G, A]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
        yield Type(Term.TypeLevel.App.`App[_[_], _]`(t.tpe, f.tpe, a.tpe))

    extension [T[_[_], _, _]] (tfunction: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _, _]`[T]])
      @scala.annotation.targetName("F[G, A, B]")
      def apply[G[_], A, B](
        farg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[G]],
        aarg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]],
        barg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[B]]
      ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[T[G, A, B]]] =
        for
          t <- tfunction
          f <- farg
          a <- aarg
          b <- barg
        yield Type(Term.TypeLevel.App.`App[_[_], _, _]`(t.tpe, f.tpe, a.tpe, b.tpe))

    extension [A, B] (function: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => B]])
      @scala.annotation.targetName("A => B")
      def apply(args: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]] =
        for
          f <- function
          a <- args
          t <- StateT.liftF[ErrorF, (Set[LibDep], List[Statement]), Term.TypeLevel.`*`[B]](f.value.tpe match
            case Term.TypeLevel.App.`App[_]`(tfun, targ) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.`App[_[_]]`(tfun, farg) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.`App[_[_], _]`(tfun, farg, aarg) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.`App[_, _]`(tfun, ta, tb) => Right(tb.asInstanceOf[Term.TypeLevel.`*`[B]])
            case Term.TypeLevel.App.`App[_, _, _]`(tfun, ta1, ta2, tb) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.`App[_[_], _, _]`(tfun, farg, aarg, barg) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.`App[_[_[_], _]]`(_, _) => Left(List(Error(s"Application Error"))) 
            case Term.TypeLevel.App.Infix(tfun, ta, tb) => Right(tb.asInstanceOf[Term.TypeLevel.`*`[B]])
            case Term.TypeLevel.App.Infix2(tfun, ta, tb, tc) => Left(List(Error(s"Application Error")))
            case Term.TypeLevel.Var.`UserDefinedType`(nme, impl) => Left(List(Error(s"Application Error")))
          )
        yield Value(Term.ValueLevel.App.App1(f.value, a.value, t))
    
    // extension [A, B] (arg1: StateT[ErrorF, (Set[LibDep], List[Statement]), Type[A]])
    //   @scala.annotation.targetName("dot1T")
    //   def DOT(func: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => B]])(arg2: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]] =
    //     for
    //       f <- StateT.liftF(func.runEmptyA)
    //       a1 <- arg1
    //       a2 <- StateT.liftF(arg2.runEmptyA)
    //       v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[B]](Term.ValueLevel.App.Dot1(f.value, a1.value, a2.value, a2.value.tpe))
    //     yield Value(v)

    extension [A, B] (arg1: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A]])
      @scala.annotation.targetName("dot1V_fa")
      def DOT(func: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[A => B]])(arg2: StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]]): StateT[ErrorF, (Set[LibDep], List[Statement]), Value[B]] =
        for
          f <- func
          a1 <- StateT.liftF(arg1.runEmptyA)
          a2 <- StateT.liftF(arg2.runEmptyA)
          v <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), Term.ValueLevel.`*`[B]](Term.ValueLevel.App.Dot1(f.value, a1.value, a2.value, a2.value.tpe))
        yield Value(v)