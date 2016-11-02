package fp.datastructs

import org.scalatest.FunSpec

/**
  * Created by apple on 10/29/16.
  */
class EitherTest extends FunSpec {

  describe("Either test spec") {

    it("test the method mean() of Either") {
      val meanValue = Either.mean(IndexedSeq(20.0, 10.0, 90.0))

      assert(meanValue.isInstanceOf[Right[Double]])
      assert(meanValue == Right(40.0))

      val empty = Either.mean(IndexedSeq())
      assert(empty.isInstanceOf[Left[String]])
    }
    it("test the function safeDiv() of Either ") {
      val result = Either.safeDiv(9, 3)
      assert(result == Right(3.0))
    }

  }
}
