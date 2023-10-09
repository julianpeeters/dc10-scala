# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.

 - [`dc10-scala`](#dc10-scala): AST and dsl for defining and rendering Scala programs  
    - -----
      <details><summary>see available libraries</summary>
    
    - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)
    
    </details>

### Getting Started
 - Library for Scala @SCALA@ (JVM only)
 - Generates code for Scala @SCALA@

```scala
"com.julianpeeters" %% "dc10-scala" % "@VERSION@"
```

### Usage

#### `dc10-scala`

Use the dsl to define Scala code:

```scala mdoc
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. "hello, world"

val snippet = 
  for
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
```

Use the compiler impl to check and render code `toString` or `toVirtualFile`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
```