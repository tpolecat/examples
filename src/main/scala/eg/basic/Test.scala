package eg.basic

import language.reflectiveCalls
import language.postfixOps

object Test extends App {

  val count = new BASIC {

    10 FOR I IN 1 TO 2
    15 FOR J IN 1 TO 3
    20 GOSUB 100
    25 NEXT J
    30 NEXT I
    40 END

    100 PRINT "Current values:"
    102 PRINT I
    105 PRINT J
    110 RETURN

  }

  count()

}

