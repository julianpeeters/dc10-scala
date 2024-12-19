# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.
 - Library for Scala @SCALA@ (JS, JVM, and Native platforms)
 - Generates code for Scala @SCALA@

```scala
"com.julianpeeters" %% "dc10-scala" % "@VERSION@"
```

### `dc10-scala`


Use the `dsl` to define Scala code (see available [libraries](#libraries)):

```scala mdoc
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. "hello, world"

val snippet = 
  for
    s <- VAL("str", STRING) := "hello, world"
    _ <- VAL("msg", STRING) := s
  yield ()
```

Use the `compiler` impl to check and render code to `string` or `virtualFile`:

```scala mdoc
import dc10.scala.compiler.{compile, string}
import dc10.scala.version.`3.5.2`

val result: String = snippet.compile.string
```

### Libraries
 - [`dc10-cats`](https://github.com/julianpeeters/dc10-cats)
 - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)