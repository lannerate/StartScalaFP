import org.scalatest.FunSpec
import scala.util.{Either => _, Right => _, Left => _, _}
import fp.datastructs.{Either, Right, Left}


class PersonTest extends FunSpec {
  describe("Person test using Either feature"){

    it("test the function mkName()"){
      val name = Person.mkName("hui")
      assert( name.isRight )
    }

    it("test the function mkAge()"){
      val age = Person.mkAge(30)
      assert( age.isRight )
    }

    it("test the function mkPerson()"){
      val person = Person.mkPerson("hui",24)

      assert( person.isInstanceOf[Either[String,Person]])
    }


  }
}
