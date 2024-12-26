package dc10.scala.metalang

import cats.data.StateT
import dc10.scala.{*, given}
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions

trait `dc10-scala`[F[_]]:
  def ERRORF: F[`Type.*->*`[ErrorF]]
  def LIBDEP: F[`Type.*`[LibDep]]
  def STATEMENT: F[`Type.*`[Statement]]
  def `TYPEEXPR`[G[_], A](targ: F[`Type.*`[A]]): F[`Type.*`[G[A]]]
  @scala.annotation.targetName("_[_[_], _]")
  def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](targ: F[`Type.(*->*)->*->*`[H]]): F[`Type.*`[G[H]]]
    @scala.annotation.targetName("Type[_[_], _]")
  def TYPEEXPR[T[_[_], _]](arg: F[`Value.*`[`Type.Var[_[_], _]`[T]]]): F[`Value.*`[`Type.(*->*)->*->*`[T]]]
  def VALUEEXPR[T](arg: F[`Value.*`[T]]): F[`Value.*`[`Value.*`[T]]]
  def `TYPEEXPR[_]`[G[_[_]], H[_]](targ: F[`Type.*->*`[H]]): F[`Type.*`[G[H]]]
  def `Type.Var[_[_], _]`[G[_[_], _]](nme: String): F[`Value.*`[`Type.Var[_[_], _]`[G]]]
  @scala.annotation.targetName("Value[_[_]]")
  def VALUEEXPR[G[_], A](targ: F[`Type.*`[A]]): F[`Type.*`[G[A]]]
  extension (ctx: `Value.*`[(Set[LibDep], List[Statement])])
    def DEP(d: F[`Value.*`[LibDep]]): F[`Value.*`[ErrorF[(Set[LibDep], List[Statement])]]]

object `dc10-scala`:

  val lib: LibDep = LibDep(BuildInfo.organization, BuildInfo.name, BuildInfo.version)

  trait Mixins extends `dc10-scala`[StateT[ErrorF, (Set[LibDep], List[Statement]), _]]:

    def ERRORF: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[ErrorF]] =
      StateT.pure(`Type.Var[_]`[ErrorF](0, "dc10.scala.ErrorF", None))

    def LIBDEP: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[LibDep]] =
      StateT.pure(`Type.Var`[LibDep](0, "dc10.scala.LibDep", None))

    def LibDep(
      org: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]],
      nme: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]],
      ver: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[String]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[LibDep]] =
      for
        o <- org
        n <- nme
        v <- ver
        t <- LIBDEP
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), (`Type.*`[String], `Type.*`[String], `Type.*`[String])]((o.tpe, n.tpe, v.tpe)) ==> t
      yield `Value.App3`(0, `Value.VarA`[(String, String, String) => LibDep](0, "dc10.scala.LibDep", f), o, n, v, t)

    def STATEMENT: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[Statement]] =
      StateT.pure(`Type.Var`[Statement](0, "dc10.scala.Statement", None))

    def `TYPEEXPR`[G[_], A](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var[_]`(0, "dc10.scala.`Type.*`", None),
          a
        )

    @scala.annotation.targetName("_[_[_], _]")
    def `TYPEEXPR`[G[_[_[_], _]], H[_[_], _]](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.(*->*)->*->*`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_[_], _]]`(
          0,
          `Type.Var[_[_[_], _]]`(0, "dc10.scala.`Type.(*->*)->*->*`", None),
          a
        )

    @scala.annotation.targetName("Type[_[_], _]")
    def TYPEEXPR[T[_[_], _]](arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Type.Var[_[_], _]`[T]]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Type.(*->*)->*->*`[T]]] =
      for
        a <- arg
        t <- StateT.pure(`Value.VarA`[`Type.(*->*)->*->*`[T]](0, "", `Type.Var`(0, "dc10.scala.`Type.Var[_[_], _]`", None)))
        f <- StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Type.Var[_[_], _]`[T]]](a) ==> (x => StateT.pure(t))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield 
        `Value.App1`(
          0,
          `Value.VarA`[`Type.Var[_[_], _]`[T] => `Type.(*->*)->*->*`[T]](0, "dc10.scala.`Type.Var[_[_], _]`", f.tpe),
          a,
          t.tpe
        )
      
    def `VALUEEXPR`[T](
      arg: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[T]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Value.*`[T]]] =
      for
        a <- arg
        t <- StateT.pure(`Type.Var`[`Value.*`[T]](0, "dc10.scala.`Type.(*->*)->*->*`", None))
        f <- arg ==> (x => StateT.pure[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Value.*`[T]]](`Value.VarA`[`Value.*`[T]](0, "SDSD", t)))
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield 
        `Value.App1`(0, f, a, t)

    def `TYPEEXPR[_]`[G[_[_]], H[_]](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*->*`[H]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[G[H]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_[_]]`(
          0,
          `Type.Var[_[_]]`(0, "dc10.scala.`Type.*->*`", None),
          a
        )

    def `Type.Var[_[_], _]`[G[_[_], _]](nme: String): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[`Type.Var[_[_], _]`[G]]] =
      StateT.pure(
        `Value.VarA`(
          0,
          "dc10.scala.`Type.Var[_[_], _]`",
          `Type.Var`(0, s"$nme**", None)
        )
      )

    @scala.annotation.targetName("Value[_[_]]")
    def VALUEEXPR[G[_], A](
      targ: StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[A]]
    ): StateT[ErrorF, (Set[LibDep], List[Statement]), `Type.*`[G[A]]] =
      for
        a <- targ
        _ <- StateT.modifyF[ErrorF, (Set[LibDep], List[Statement])](ctx => ctx.dep(`dc10-scala`.lib))
      yield `Type.App[_]`(
          0,
          `Type.Var[_]`(0, "dc10.scala.`Value.*`", None),
          a
        )

    extension (ctx: `Value.*`[(Set[LibDep], List[Statement])])
      def DEP(d: StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[LibDep]]): StateT[ErrorF, (Set[LibDep], List[Statement]), `Value.*`[ErrorF[(Set[LibDep], List[Statement])]]] =
        for
          f <- VAL("dep", ctx.tpe ==> ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT))))
          t <- ERRORF(TUPLE(SET(LIBDEP), LIST(STATEMENT)))
          a <- d
        yield `Value.AppDot1`(0, f, ctx, a, t)