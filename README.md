# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.

 - [`dc10-scala`](#dc10-scala): AST and dsl for defining and rendering Scala programs  
    - -----
      <details><summary>see available libraries</summary>
    
    - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)
    
    </details>

### Getting Started
 - Library for Scala 3 (JVM only)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scala" % "0.2.0"
```

### Usage

#### `dc10-scala`

Use the dsl to define Scala code:

```scala
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. "hello, world"

val snippet = 
  for
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], Unit] = cats.data.IndexedStateT@2ff14e33
```

Use the compiler impl to check and render code `toString` or `toVirtualFile`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
// result: String = """val str: String = "hello, world"
// val msg: String = str"""
```