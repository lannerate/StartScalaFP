package fp.currying

/**
  * Created by apple on 11/13/16.
  */
object Filter {

 def filter(xs:List[Int], f:Int => Boolean):List[Int] =
  if(xs.isEmpty) xs
  else if (f(xs.head)) xs.head :: filter(xs.tail,f)
  else filter(xs.tail,f)

 def divideBy(n:Int)(m:Int) = (m % n == 0)


}
