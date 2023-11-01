# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.
 - Library for Scala @SCALA@ (JVM only)
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
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
```

Use the `compiler` impl to check and render code `toString` or `toVirtualFile`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
```

### `dc10-scalaq`
Experimental `dsl` that includes dependent vector types:

```scala mdoc:reset
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. 1, 2, 3

val snippet = 
  for
    l <- VAL("l", VECTOR(3, INT), Vector.of(1, 2, 3))
    _ <- VAL("m", VECTOR(6, INT), l ++ l)
  yield ()
```

Use the `compiler` to typecheck, then render code as `List`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
```

### Libraries
 - [`dc10-cats`](https://github.com/julianpeeters/dc10-cats)
 - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)
