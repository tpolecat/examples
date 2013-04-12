package eg.basic

import language._
import Variants._

object VariantTest {

  Variant(3)                                      //> res0: eg.basic.Variants.Variant = VNumber(-\/(3))
  Variant(3.0 * 2)                                //> res1: eg.basic.Variants.Variant = VNumber(-\/(6))

  Variant(3) =? Variant(3.0)                      //> res2: scalaz.Validation[eg.basic.Errors.Error,eg.basic.Variants.Variant] = S
                                                  //| uccess(VNumber(-\/(-1)))

  Variant(2) =? Variant("abc")                    //> res3: scalaz.Validation[eg.basic.Errors.Error,eg.basic.Variants.Variant] = F
                                                  //| ailure(TM Type Mismatch: Expected Numeric, found String.)
  Variant("foo") + Variant("bar")                 //> res4: scalaz.Validation[eg.basic.Errors.Error,eg.basic.Variants.Variant] = S
                                                  //| uccess(VString(foobar))

  val a = Variant(1) / Variant(2)                 //> a  : scalaz.Validation[eg.basic.Errors.Error,eg.basic.Variants.Variant] = Su
                                                  //| ccess(VNumber(\/-(0.5)))
  a.flatMap(_ * Variant(4))                       //> res5: scalaz.Validation[eg.basic.Errors.Error,eg.basic.Variants.Variant] = S
                                                  //| uccess(VNumber(-\/(2)))

  new BASIC {

    10 LET A$ := "foo"
    20 PRINT B$

    RUN

  }                                               //> 
                                                  //| res6: eg.basic.BASIC = eg.basic.VariantTest$$anonfun$main$1$$anon$1@35f03691
                                                  //| 
}