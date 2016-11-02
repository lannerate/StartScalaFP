import scala.util.{Either => _, Right => _, Left => _, _}
import fp.datastructs.{Either, Right, Left}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {

  def mkName(name: String): Either[String, Name] =
    if (name == null || name == "") Left("name is empty")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age <= 0) Left("Age is illegal")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
