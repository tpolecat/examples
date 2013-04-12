Embedded BASIC
==============

This is an embedded DSL and monadic interpreter for the execrable BASIC language. It's useless but kind of interesting. Still a work in progress, but you can write real programs as things stand. The `examples/` directory has a playable game that I typed in from a book written in 1978. 

An Example
------------------

This is Scala code.

```scala
import eg.basic.BASIC

object HelloWorld extends App {

  val b = new BASIC {

    10 PRINT "HELLO WORLD"
    20 INPUT "WHAT IS YOUR NAME?" AS N$
    30 INPUT "HOW MANY TIMES SHALL I PRINT IT?" AS N
    40 IF N <> INT(N) THEN 30
    50 IF N < 1 THEN 30
    60 PRINT "OK THEN:"
    70 FOR I IN 1 TO N
    80 PRINT N$
    90 NEXT I
    95 PRINT "BYE."

  }

  b.run()

}
```



