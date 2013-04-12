BASIC
========

This started out as a really dumb experiment, and it's still dumb but it's kind of interesting. It's an _embedded_ DSL for the execrable BASIC language, with a nice monadic interpreter.

THIS IS SCALA CODE
------------------

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
``

