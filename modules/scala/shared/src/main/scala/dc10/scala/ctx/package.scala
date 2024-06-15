package dc10.scala.ctx

import cats.{Applicative, Functor}
import dc10.scala.{File, LibDep, Statement}

extension [F[_]: Applicative: Functor] (ctx: List[Statement.`package`])
  def ext(s: Statement.`package`): F[List[Statement.`package`]] =
    Functor[F].map(namecheck(s))(ctx :+ _)
  def namecheck(s: Statement.`package`): F[Statement.`package`] =
    // TODO
    Applicative[F].pure(s)

extension [F[_]: Applicative: Functor](ctx: (Set[LibDep], List[Statement]))
  def dep(d: LibDep): F[(Set[LibDep], List[Statement])] =
    Applicative[F].pure((ctx._1 + d, ctx._2))
  def ext(s: Statement): F[(Set[LibDep], List[Statement])] =
    Functor[F].map(namecheck(s))(stmt => (ctx._1, ctx._2 :+ stmt))
  def namecheck(s: Statement): F[Statement] =
    // TODO
    Applicative[F].pure(s)

extension [F[_]: Applicative: Functor](ctx: List[Statement.ValueDef])
  def ext(s: Statement.ValueDef): F[List[Statement.ValueDef]] =
    Functor[F].map(namecheck(s))(ctx :+ _)
  def namecheck(s: Statement.ValueDef): F[Statement.ValueDef] =
    // TODO
    Applicative[F].pure(s)

extension [F[_]: Applicative: Functor] (ctx: (Set[LibDep], List[File]))
  @scala.annotation.targetName("depFile")
  def dep(d: LibDep): F[(Set[LibDep], List[File])] =
    Applicative[F].pure((ctx._1 + d, ctx._2))
  def ext(s: File): F[(Set[LibDep], List[File])] =
    Functor[F].map(namecheck(s))(stmt => (ctx._1, ctx._2 :+ stmt))
  def namecheck(s: File): F[File] =
    // TODO
    Applicative[F].pure(s)