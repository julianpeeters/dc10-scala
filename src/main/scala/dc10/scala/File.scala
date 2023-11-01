package dc10.scala

import java.nio.file.Path

case class File(path: Path, contents: List[Statement])
object File:

  extension (file: File)
    def addParent(path: Path): File =
      file.copy(path = path.resolve(file.path))