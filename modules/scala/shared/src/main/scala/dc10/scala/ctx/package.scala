package dc10.scala.ctx

import cats.{Applicative, Functor}
import dc10.scala.{File, Statement}
import dc10.scala.Statement.{LibraryDependency, PackageDef}

extension [F[_]: Applicative: Functor] (ctx: List[PackageDef])
  def ext(s: PackageDef): F[List[PackageDef]] =
    Functor[F].map(namecheck(s))(ctx :+ _)
  def namecheck(s: PackageDef): F[PackageDef] =
    // TODO
    Applicative[F].pure(s)

extension [F[_]: Applicative: Functor](ctx: (Set[LibraryDependency], List[Statement]))
  def dep(d: LibraryDependency): F[(Set[LibraryDependency], List[Statement])] =
    Applicative[F].pure((ctx._1 + d, ctx._2))
  def ext(s: Statement): F[(Set[LibraryDependency], List[Statement])] =
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

extension [F[_]: Applicative: Functor] (ctx: (Set[LibraryDependency], List[File]))
  @scala.annotation.targetName("depFile")
  def dep(d: LibraryDependency): F[(Set[LibraryDependency], List[File])] =
    Applicative[F].pure((ctx._1 + d, ctx._2))
  def ext(s: File): F[(Set[LibraryDependency], List[File])] =
    Functor[F].map(namecheck(s))(stmt => (ctx._1, ctx._2 :+ stmt))
  def namecheck(s: File): F[File] =
    // TODO
    Applicative[F].pure(s)