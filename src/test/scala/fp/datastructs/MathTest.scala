package fp.datastructs

import org.scalatest.FunSpec

/**
  * Created by apple on 10/13/16.
  */
class MathTest extends FunSpec{

  describe("Test some functions of Math"){

    it("Math.mean(Seq(2,4,8,10)) should be equal to 6"){
      val mean = Math.mean(Seq(2,4,8,10))
      assert(mean.get == 6)
    }

    it("Math.variance(Seq(2,4,8,10)) should be equal to 10"){
      val variance = Math.variance(Seq(2,4,8,10)).get
      assert(variance == 10)
    }
  }
}
