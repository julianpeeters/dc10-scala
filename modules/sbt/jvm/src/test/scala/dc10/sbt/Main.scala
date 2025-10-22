package dc10.sbt

import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions

val `Main.scala` =
  FILE"Main.scala" {
    PACKAGE"example" {

      VAL"hello"$ String := "hello, world"

    }
  }
