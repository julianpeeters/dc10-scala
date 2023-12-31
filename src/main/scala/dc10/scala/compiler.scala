package dc10.scala

import cats.data.StateT
import dc10.compile.{Compiler, Renderer, VirtualFile}

implicit object compiler extends Compiler[
  ErrorF,
  List,
  Error,
  Statement,
  File
]:

  type Ctx[F[_], L, A] = StateT[F, L, A]

  extension [C, D] (ast: StateT[ErrorF, List[D], C])
    def compile: ErrorF[List[D]] =
      ast.runEmptyS

  extension (res: ErrorF[List[Statement]])
    def toString[V](
      using R: Renderer[V, Error, List[Statement]]
    ): String =
      res.fold(R.renderErrors, R.render)

  extension (res: ErrorF[List[Statement]])
    def toStringOrError[V](
      using R: Renderer[V, Error, List[Statement]]
    ): ErrorF[String] =
      res.map(R.render)

  extension (res: ErrorF[List[File]])
    def toVirtualFile[V](
      using R: Renderer[V, Error, List[Statement]]
    ): ErrorF[List[VirtualFile]] =
      for
        fds <- res
      yield fds.map(fileDef =>
          VirtualFile(
            fileDef.path,
            R.render(fileDef.contents)
          )
        )