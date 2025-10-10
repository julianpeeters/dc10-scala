package dc10.scala.predef.datatype

import cats.Id
import cats.data.StateT
// import cats.syntax.all.given
import dc10.scala.dsl.{given}
import dc10.scala.*
// import dc10.scala.predef.calculus.Applications.applyType
// import dc10.scala.predef.binding.Assignments.{:=}
import dc10.scala.predef.calculus.Functions.{function1}
import dc10.scala.predef.calculus.Functions.typeLambda1
import dc10.scala.predef.calculus.variables.Variables.{A}

import scala.language.implicitConversions
import dc10.scala.predef.datatype.PrimitiveTypes.{nothing}
// import dc10.scala.predef.calculus.Functions.typeLambda1
// import dc10.scala.predef.calculus.Functions.typeLambda1

trait ComplexTypes[F[_]]:

  def LIST: F[`Type.Var: x→x`[List]]
  def List: F[`Value: x`[scala.List.type]]
  extension (l: F[`Value: x`[scala.List.type]])
    @scala.annotation.targetName("applyList")
    def apply: F[`Value.x→x`[[A] =>> List[A] => List[A]]]

  def None[A]: F[`Value.Var`[Option[A]]]
  
  def OPTION: F[`Type.Var: x→x`[[A] =>> Option[A]]]
  def Option: F[`Value: x`[scala.Option.type]]
  extension (o: F[`Value: x`[scala.Option.type]])
    @scala.annotation.targetName("applyOption")
    def apply: F[`Value.x→x`[[A] =>> A => Option[A]]]

  def SET: F[`Type.Var: x→x`[Set]]
  def Set: F[`Value: x`[scala.Predef.Set.type]]
  extension (s: F[`Value: x`[scala.Predef.Set.type]])
    @scala.annotation.targetName("applySet")
    def apply: F[`Value.x→x`[[A] =>> List[A] => Set[A]]]

  def Some: F[`Value.x→x`[[A] =>> A => Option[A]]]
  def TUPLE: F[`Type.Var: x→x→x`[Tuple2]]
  def Tuple[A, B]: (F[`Value: x`[A]], F[`Value: x`[B]]) => F[`Value: x`[Tuple2[A, B]]]
  def TUPLE3: F[`Type.x→x→x→x`[Tuple3]]
  def Tuple3[A, B, C]: (F[`Value: x`[A]], F[`Value: x`[B]], F[`Value: x`[C]]) => F[`Value: x`[Tuple3[A, B, C]]]

object ComplexTypes:

  def option[A]: `Type.Var: x→x`[Option] =
    `Type.Var: x→x`[Option](0, "Option", scala.None, () => scala.List(some, none))
  

  // TODO
  def option[A](targ: `Type.Var: x`[A]): `Type: x`[Option[A]] =
    `Type.App[_]`(0, option, A)

  def none[A]: `Value.Var.Unbound.Data`[Option[Nothing]] =
    `Value.Var.Unbound.Data`(0, "None", option(nothing))

  def some[A]: `Value.x→x`[[A] =>> Id[A] => Option[A]] =
    `Value.Var1[_]`(
      0,
      "Some",
      typeLambda1[[A] =>> Id[A] => Option[A], A](A, function1(A, option(A))),
      scala.None
    )

  // extension (o: StateT[ErrorF, Γ, `Value.x→x`[[A] =>> Id[A] => Option[A]]])
  //   def unapply[B[_]](x: `Value.x→x`[B]) = scala.Some(StateT.pure[ErrorF, Γ, `Value.x→x`[B]](x))
    // Id(`function1[_]`[[A] =>> A => Option[A], A](
    //   A,
    //   `Value.Var.Unbound.Data`[A => Option[A]](0, "Some", function1(`A`, `Option[A]`))
    // )
    // ).map(c => `Value.Var1[_]`[Id, Option](0, "Some", c.tpe, scala.Some(c)))
    
    // for
    //   // `Option[A]` <- option.applyType(A[A])
    //   f <- Right(`Value.Var.Unbound.Data`[A => Option[A]](0, "Some", function1(`A`, `Option[A]`)))
    //   // a <- Right("x" :: A[A])
    //   // b <- Right(`Value.App.1: x`(0, f, a, `Option[A]`))
    //   // c <- Right(`function1[_]`[[A] =>> A => Option[A], A](A, function1(a, b)))
    //   c <- Right(`function1[_]`[[A] =>> A => Option[A], A](A, f))
    // yield `Value.Var1[_]`[Id, Option](0, "Some", c.tpe, scala.Some(c))


  // def someA: 
      // val f  = `Value.Lam.1: x→x`[[A] =>> A => Option[A], A](
      //   0,
      //   function1[A, Option[A]](
      //       "x" :: A,
      //       `Value.App.1: x`[A, Option[A]](0, `Value.Var.Unbound.Data`(0, "Some", function1(A[A], t)), "x" :: A, t)
      //     ),
      //   typeLambda1[[A] =>> A => Option[A], A](
      //     A[A],
      //     function1(A[A], t)
      //     // ???//function1(targ, `Type.Var: x→x`[Option](0, "Option", scala.None, scala.List()).applyType(targ))
      //   )
      // )

      // `Value.Var1[_]`[Id, Option](
      //   0,
      //   "Some",
      //   f.tpe,
      //   scala.Some(f)
      // )
        // function1[[A] =>> A => Option[A], A](
        //   A,
        //   "x" :: A,
        //   // function1[A, Option[A]](`Value.Var.Unbound.Data`[A](0, "x", a), ???)
          // function1[A, Option[A]](
          //   "x" :: A,
          //   `Value.App.1: x`[A, Option[A]](0, `Value.Var.Unbound.Data`(0, "Some", function1(A[A], t)), "x" :: A, t)
          // )
      
        // function1[[A] =>> A => Option[A], A](
        //   // "Some",
        //   `Type.Var: x`[A](0, "A", scala.None),
        //   function1[A, Option[A]](`Value.Var.Unbound.Data`[A](0, "x", `Type.Var: x`[A](0, "A", scala.None)), ???)
        // )
        // `Value.Var1[_]`[Id, Option](
        //   0,
        //   "Some",
        //   typeLambda1[[A] =>> A => Option[A], A](
        //     `Type.Var: x`[A](0, "A", scala.None),
        //     function1(`Type.Var: x`[A](0, "A", scala.None), t)
        //     // ???//function1(targ, `Type.Var: x→x`[Option](0, "Option", scala.None, scala.List()).applyType(targ))
        //   ),
        //   scala.Some(
        //     function1[[A] =>> A => Option[A], A](
        //       `Type.Var: x`[A](0, "A", scala.None),
        //       function1[A, Option[A]](`Value.Var.Unbound.Data`[A](0, "x", `Type.Var: x`[A](0, "A", scala.None)), ???)
        //     )
        //   )
        // )



    // )
    // )

  trait Mixins extends ComplexTypes[StateT[ErrorF, Γ, _]]:
      
    def LIST: StateT[ErrorF, Γ, `Type.Var: x→x`[List]] =
      StateT.pure(`Type.Var: x→x`[List](0, "List", scala.None, () => scala.Nil))

    def List: StateT[ErrorF, Γ, `Value: x`[scala.List.type]] =
      StateT.pure(`Value.Var.Unbound.Data`(0, "List", `Type.Var: x`(0, "List.type", scala.None)))

    extension (l: StateT[ErrorF, Γ,`Value: x`[scala.List.type]])
      @scala.annotation.targetName("applyList")
      def apply: StateT[ErrorF, Γ, `Value.x→x`[[A] =>> List[A] => List[A]]] =
        ???
        // for
        //   t <- TYPE("A").==>>[[A] =>> List[A] => List[A]](a => dsl.apply(LIST)(refT(a)) ==> dsl.apply(LIST)(a))
        // yield `Value.Var1[_]`(0, "List", t, scala.None)

    def None[A]: StateT[ErrorF, Γ, `Value.Var`[Option[A]]] =
      ???
      // dsl.apply(OPTION)(NOTHING[Nothing]).map(t => `Value.Var.Unbound.Data`(0, "None", t))
      // for
      //   t <- dsl.apply(OPTION)(NOTHING)
      // yield `Value.Var.Unbound.Data`[Option[A]](0, "None", t)
       
    def OPTION: StateT[ErrorF, Γ, `Type.Var: x→x`[[A] =>> Option[A]]] =
      for
        s <- Some
        // n <- None
      // yield `Type.Var: x→x`(0, "Option", scala.None)
      yield 
        `Type.Var: x→x`(0, "Option", scala.None, () => scala.List(s))

    def Option: StateT[ErrorF, Γ, `Value: x`[scala.Option.type]] =
      StateT.pure(`Value.Var.Unbound.Data`(0, "Option", `Type.Var: x`(0, "Option.type", scala.None)))

    extension (o: StateT[ErrorF, Γ,`Value: x`[scala.Option.type]])
      @scala.annotation.targetName("applyOption")
      def apply: StateT[ErrorF, Γ, `Value.x→x`[[A] =>> A => Option[A]]] =
        ???

        // for
        //   t <- TYPE("A").==>>[[A] =>> A => Option[A]](a => refT(a) ==> dsl.apply(OPTION)(a))
        // yield `Value.Var1[_]`[Id, Option](0, "Option", t, scala.None)

    def SET: StateT[ErrorF, Γ, `Type.Var: x→x`[Set]] =
      StateT.pure(`Type.Var: x→x`[Set](0, "Set", scala.None, () => Nil))

    def Set: StateT[ErrorF, Γ, `Value: x`[scala.Predef.Set.type]] =
      StateT.pure(`Value.Var.Unbound.Data`(0, "Set", `Type.Var: x`(0, "Set.type", scala.None)))

    extension (l: StateT[ErrorF, Γ,`Value: x`[scala.Predef.Set.type]])
      @scala.annotation.targetName("applySet")
      def apply: StateT[ErrorF, Γ, `Value.x→x`[[A] =>> List[A] => Set[A]]] =
        ???
        // for
        //   t <- TYPE("A").==>>[[A] =>> List[A] => Set[A]](a => dsl.apply(LIST)(refT(a)) ==> dsl.apply(SET)(a))
        // yield `Value.Var1[_]`(0, "Set", t, scala.None)

   
    def Some: StateT[ErrorF, Γ, `Value.x→x`[[A] =>> A => Option[A]]] =
      StateT.pure(some)
    
      // for
      //   // _ <- StateT.pure(())//TYPE("A").==>>[[A]=>> Id[A] => Option[A]](a => dsl.apply(OPTION)(a).map(o => function1(a, o)))
      //   t <- TYPE("A").==>>[[A]=>> Id[A] => Option[A]](a => StateT.liftF(option.applyType(a).map(o => function1(a, o))))
      // yield `Value.Var1[_]`[Id, Option](0, "Some", t, scala.None)

    def TUPLE: StateT[ErrorF, Γ, `Type.Var: x→x→x`[Tuple2]] =
      StateT.pure(`Type.Var: x→x→x`(0, "Tuple2", scala.None))
      
    def Tuple[A, B]: (
      StateT[ErrorF, Γ, `Value: x`[A]],
      StateT[ErrorF, Γ, `Value: x`[B]]
    ) => StateT[ErrorF, Γ, `Value: x`[Tuple2[A, B]]] =
      (arg1, arg2) =>
        for
          a <- arg1
          b <- arg2
          t <- dsl.apply(TUPLE)(a.tpe, b.tpe)
          v <- StateT.pure[ErrorF, Γ, `Value: x`[Tuple2[A, B]]](
            `Value.App.2: x`(
              0,
              `Value.Var.Unbound.Data`(0, "", `Type.App[_, _, _]`(0, `Type.Var: x→x→x→x`(0, "=>", scala.None), a.tpe, b.tpe, t)),
              a,
              b,
              t
            )
          )
        yield v

    def TUPLE3: StateT[ErrorF, Γ, `Type.x→x→x→x`[Tuple3]] =
      StateT.pure(`Type.Var: x→x→x→x`(0, "Tuple3", scala.None))

    def Tuple3[A, B, C]: (
      StateT[ErrorF, Γ, `Value: x`[A]],
      StateT[ErrorF, Γ, `Value: x`[B]],
      StateT[ErrorF, Γ, `Value: x`[C]]
    ) => StateT[ErrorF, Γ, `Value: x`[Tuple3[A, B, C]]] =
      (arg1, arg2, arg3) =>
        for
          a <- arg1
          b <- arg2
          c <- arg3
          t <- dsl.apply(TUPLE3)(a.tpe, b.tpe, c.tpe)
          v <- StateT.pure[ErrorF, Γ, `Value: x`[Tuple3[A, B, C]]](
            `Value.App.3: x`(
              0,
              `Value.Var.Unbound.Data`(0, "", `Type.App[_, _, _, _]`(0, `Type.Var: x→x→x→x→x`(0, "=>", scala.None), a.tpe, b.tpe, c.tpe, t)),
              a,
              b,
              c,
              t
            )
          )
        yield v