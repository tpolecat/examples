package eg

import org.specs2.mutable.Specification

class SpecsExample extends Specification {

  "Specs Example" should {
    
    "Add numbers correctly" in {
      (1 + 2) must_== 3
    }
    
    "hmm" in {
      1 must_== 1
    }
    
  }
  
}