package dc10.scala.metalang

import cats.data.StateT
import dc10.scala.{ErrorF, LibDep, Statement, compiler}
import dc10.scala.Statement.TypeExpr.{`Type`, `Type[_]`, `Type[_[_], _]`}
import dc10.scala.Statement.ValueExpr.{`Value`, `Value[_[_], _]`}
import dc10.scala.Symbol.Term
// import dc10.scala.Symbol.Term.ValueLevel.Var.`UserDefinedValue[_[_], _]`

trait `dc10-scala`[F[_]]:
  def ERRORF: F[`Type[_]`[ErrorF]]
  def LIBDEP: F[`Type`[LibDep]]
  def STATEMENT: F[`Type`[Statement]]
  def `TYPEEXPR`[G[_], A](targ: F[`Type`[A]]): F[`Type`[G[A]]]
  @scala.annotation.targetName("_[_[_], _]")
  def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](targ: F[`Type[_[_], _]`[H]]): F[`Type`[G[H]]]
  // @scala.annotation.targetName("Value[_[_], _]")
  // def `TYPEEXPR`[G[_[_[_], _]], T[_[_], _]](arg: F[`Value[_[_], _]`[T]]): F[`Value`[G[T]]]
  def `TYPEEXPR[_]`[G[_[_]], H[_]](targ: F[`Type[_]`[H]]): F[`Type`[G[H]]]
  def USERDEFINEDTYPE[T](nme: String): F[`Value`[T]]
  @scala.annotation.targetName("UserDefinedType[_[_], _]")
  def USERDEFINEDTYPE[G[_[_], _]](nme: String): F[`Value[_[_], _]`[G]]
  def VALUEEXPR[G[_], A](targ: F[`Type`[A]]): F[`Type`[G[A]]]

object `dc10-scala`:

  val lib: LibDep = LibDep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends `dc10-scala`[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    def ERRORF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[ErrorF]] =
      StateT.pure(`Type[_]`(Term.TypeLevel.Var.`UserDefinedType[_]`[ErrorF]("dc10.scala.ErrorF", None)))

    def LIBDEP: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[LibDep]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`[LibDep]("dc10.scala.LibDep", None)))

    def STATEMENT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[Statement]] =
      StateT.pure(`Type`(Term.TypeLevel.Var.`UserDefinedType`[Statement]("dc10.scala.Statement", None)))

    def `TYPEEXPR`[G[_], A](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield Type(
        Term.TypeLevel.App.`App[_]`(
          Term.TypeLevel.Var.`UserDefinedType[_]`("dc10.scala.Statement.TypeExpr.`Type`", None),
          a.tpe
          )
        )

    @scala.annotation.targetName("_[_[_], _]")
    def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield Type(
        Term.TypeLevel.App.`App[_[_[_], _]]`(
          Term.TypeLevel.Var.`UserDefinedType[_[_[_], _]]`("dc10.scala.Statement.TypeExpr.`Type[_[_], _]`", None),
          a.tpe
        )
      )

    // @scala.annotation.targetName("Value[_[_], _]")
    // def `TYPEEXPR`[G[_[_[_], _]], T[_[_], _]](
    //   arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value[_[_], _]`[T]]
    // ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[G[T]]] =
    //   for
    //     a <- arg
    //     // f <- a ==> a
    //     _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
    //   yield `Value`[G[T]](
    //     // Term.ValueLevel.App.`App1`(
    //       Term.ValueLevel.Var.`UserDefinedValue`[G[T]]("dc10.scala.Statement.TypeExpr.`Type[_[_], _]`",
    //       a.value match
    //         case `UserDefinedValue[_[_], _]`(nme, tpe, impl) => tpe
    //       , None)
    //     // )
    //   )

        

    // @scala.annotation.targetName("[_[_], _]")
    // def `TYPEEXPR`[G[_[_[_], _]], A[_[_], _]](targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[A]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[A]]] =
    //   for
    //     a <- targ
    //     _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
    //   yield Type(
    //     Term.TypeLevel.App.`App[_[_[_], _]]`(
    //       Term.TypeLevel.Var.`UserDefinedType[_[_[_], _]]`("dc10.scala.Statement.TypeExpr.`Type`", None),
    //       a.tpe
    //       )
    //     )

    // def `TYPEEXPR`[G[_[_], _]]: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_[_], _]`[G]] =
    //   for
    //     _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
    //   yield `Type[_[_], _]`(
    //       Term.TypeLevel.Var.`UserDefinedType[_[_], _]`("dc10.scala.Statement.TypeExpr.`Type[_[_], _]`", None)
          
    //     )

    def `TYPEEXPR[_]`[G[_[_]], H[_]](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type[_]`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield Type(
        Term.TypeLevel.App.`App[_[_]]`(
          Term.TypeLevel.Var.`UserDefinedType[_[_]]`("dc10.scala.Statement.TypeExpr.`Type[_]`", None),
          a.tpe
          )
        )

    def USERDEFINEDTYPE[T](nme: String): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value`[T]] =
      StateT.pure(`Value`(
        Term.ValueLevel.Var.UserDefinedValue(
          nme,
          Term.TypeLevel.Var.`UserDefinedType`[T](nme, None),
          None
        )
      ))

    @scala.annotation.targetName("UserDefinedType[_[_], _]")
    def USERDEFINEDTYPE[G[_[_], _]](nme: String): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value[_[_], _]`[G]] =
      StateT.pure(`Value[_[_], _]`(
        Term.ValueLevel.Var.`UserDefinedValue[_[_], _]`[G](
          nme,
          Term.TypeLevel.Var.`UserDefinedType[_[_], _]`("dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType[_[_], _]", None),
          None
        )
      ))

    def VALUEEXPR[G[_], A](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield Type(
        Term.TypeLevel.App.`App[_]`(
          Term.TypeLevel.Var.`UserDefinedType[_]`("dc10.scala.Statement.ValueExpr.`Value`", None),
          a.tpe
          )
        )
      