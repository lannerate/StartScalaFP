case class Person(name:Name,age:Age)

sealed class Name(val value:String)
sealed class Age(val value:Int)

object Person {

def mkName(name:String):Either[String, Name] =
  if (name == null || name == "") Left("name is empty")
  else Right(new Name(name))

}
