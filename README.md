# dc10
A ***D**efinitional* ***C**ompiler* for defining and generating Scala code.
 - `dc10-compile`: Evaluate metaprograms into code strings
 - `dc10-io`: Evaluate metaprograms into source files
 - `dc10-scala`: AST and dsl for constructing metaprograms

### Add the dependencies
 - Libraries for Scala 3 (JVM only)

```
"com.julianpeeters" %% "dc10-compile" % "0.0.0",
"com.julianpeeters" %% "dc10-io"      % "0.0.0",
"com.julianpeeters" %% "dc10-scala"   % "0.0.0",
```

### Usage

#### `dc10-scala`

Use the dsl to define Scala code:

```scala
val snippet: StateT[ErrorF, List[Statement], Unit] = 
  for
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
```
