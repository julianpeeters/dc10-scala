package dc10.scala

object keyword:

  extension (nme: StringContext)

    def CASECLASS(args: Any*): (AliasSym, `DefSym.0`) =
      (AliasSym(args.mkString + nme.parts.mkString), `DefSym.0`(nme.parts.mkString))

    def DEF(args: Any*): `DefSym.0` =
      `DefSym.0`(args.mkString + nme.parts.mkString)

    def FILE(args: Any*): FileSym =
      FileSym(nme.raw(args*))

    def OBJECT(args: Any*): ObjSym =
      ObjSym(nme.raw(args*))

    def PACKAGE(args: Any*): PkgSym =
      PkgSym(args.mkString + nme.parts.mkString)

    def TYPE(args: Any*): AliasSym =
      AliasSym(nme.raw(args*))

    def VAL(args: Any*): ValSym =
      ValSym(args.mkString + nme.parts.mkString)
