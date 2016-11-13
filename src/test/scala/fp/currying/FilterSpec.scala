package fp.currying

import org.scalatest.FunSpec
import fp.currying.Filter._
/**
  * Created by apple on 11/13/16.
  */
class FilterSpec extends FunSpec{

  describe("Test Filter spec"){
    it("test the functional filter() of Filter"){
      val numbers = List(1,2,3,4,5,6,7,8,9,10)
      assert(List(2,4,6,8,10) == filter( numbers, divideBy(2) ))
    }
  }

}
