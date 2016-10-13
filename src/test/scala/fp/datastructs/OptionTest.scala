package fp.datastructs

import org.scalatest.FunSpec

/**
  * Created by hzhang3 on 10/13/2016.
  */
class OptionTest extends FunSpec{

  describe("test Option function"){

    it("test the function mean() using Option[A]"){
      val mean = Option.mean(Seq(100,273,35,199,200))
      assert(mean == Some(161.4))
    }
  }
}
