package dc10.sbt

object keyword:

  extension (nme: StringContext)

    // def CASECLASS(args: Any*): (AliasSym, `DefSym.0`) =
    //   (AliasSym(nme.parts.mkString), `DefSym.0`(nme.parts.mkString))

    // def DEF(args: Any*): `DefSym.0` =
    //   `DefSym.0`(nme.parts.mkString)

    // def FILE(args: Any*): FileSym =
    //   FileSym(nme.raw(args*))

    // def OBJECT(args: Any*): ObjSym =
    //   ObjSym(nme.raw(args*))

    // def PACKAGE(args: Any*): PkgSym =
    //   PkgSym(args.mkString + nme.parts.mkString)

    // def TYPE(args: Any*): AliasSym =
    //   AliasSym(nme.raw(args*))
    def BASEDIR(args: Any*): RepoSym =
      RepoSym(nme.raw(args*))