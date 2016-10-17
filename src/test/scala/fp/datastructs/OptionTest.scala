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
      val v = None.flatMap(_ => Some(23.8)).getOrElse(10)
      assert(v == 10)

      val v2 = Some(23).getOrElse(45)
      assert(v2 == 23)
    }

    it("test the function orElse() for Option"){
      val v = Some(34).orElse(Some(233)).get
      assert(v == 34)

      val v2 = None.orElse(Some(233)).get
      assert(v2 == 233)
    }

    it("test the function filter() for Option"){
      val v = Some(22).filter(_ % 2 == 0).getOrElse(0)
      assert(v == 22)

      val v2 = None.filter( _== null )
      assert(v2 == None)
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
      val matched = Options.doesMatches("\\d{4}-\\d{7}", "0231-7788333")
      assert(matched == true)
    }
  }
}
