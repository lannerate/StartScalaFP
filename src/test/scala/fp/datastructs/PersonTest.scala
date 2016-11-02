import org.scalatest.FunSpec

class PersonTest extends FunSpec {
  describe("Person test using Either feature"){

    it("test the function mkName()"){
      val name = Person.mkName("hui")
      assert( name.isRight )
    }
  }
}
