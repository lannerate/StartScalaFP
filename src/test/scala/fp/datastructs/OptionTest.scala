package fp.datastructs

import fp.datastructs
import org.scalatest.FunSpec

/**
  * Created by hzhang3 on 10/13/2016.
  */
class OptionTest extends FunSpec{

  describe("test Option function"){

    it("test the function map() for Option"){
      val maped = Some(123).map(_ * 2 ).get
      assert(maped == 246)
    }

    it("test the function flatMap() for Option"){
      val v = Some(340).flatMap(_ => Some( 800.45 )).get
      assert(v == 800.45)
    }

    it("test the function getOrElse() for Option"){
      val v2 = Some(23).getOrElse(45)
      assert(v2 == 23)
    }

    it("test the function orElse() for Option"){
      val v = Some(34).orElse(Some(233)).get
      assert(v == 34)
    }

    it("test the function filter() for Option"){
      val v = Some(22).filter(_ % 2 == 0).getOrElse(0)
      assert(v == 22)
    }

    it("test the function pattern() for Options, Start with the specific char sequence"){
      val pattern = Options.pattern("^Star").get.matcher("Starting, GOOD, String").find();
      assert(pattern == true)
    }

    it("verify telephone regex using the function mkMatcher()"){
      val matched = Options.mkMatcher("\\d{3,4}-\\d{7,8}").get.apply("0755-28792686")
      assert(matched == true)
    }

    it("verify phone format using the function mkMatcher_1()"){
      val matched = Options.mkMatcher_1("\\d{3,4}-\\d{8}").get.apply("0987-23838099")
      assert(matched == true)
    }

    it("Does phone format matches the regex using the function doesMatches()"){
      val matched = Options.doesMatches("\\d{4}-\\d{7}", "0231-7788333").getOrElse(false)
      assert(matched == true)
    }

    it("Does the phone format match both regex using the function bothMatch()"){
      val bothMatched = Options.bothMatch("\\d{4}-\\d{7}","\\d{3,4}-\\d{7,8}","0231-7788333").get
      assert(bothMatched == true)
    }

    it("Does the phone format matched the both regex using the function bothMatch_1()"){
      val bothMatched = Options.bothMatch_1("\\d{3,4}-\\d{7}","\\d{3}-\\d{7,8}","089-5678333").get
      assert(bothMatched == true)
    }


    it("Does the phone format matched the both regex using the function bothMatch_2()"){
      val bothMatched = Options.bothMatch_2("\\d{3,4}-\\d{7}","\\d{3}-\\d{7,8}","089-5678333").get
      assert(bothMatched == true)
    }

    it("Test Options.sequence(List(Some(1), Some(Nil), Some(2), Some(3))) should return List(1, Nil, 2, 3)"){
      val sequence = Options.sequence(List(Some(1), Some(Nil), Some(2), Some(3))).get
      assert(sequence == List(1, Nil, 2, 3))
    }

  }
}
