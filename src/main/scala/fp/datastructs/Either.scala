package fp.datastructs


sealed trait Either[+E, +A] {
  def isRight:Boolean

  def isLeft:Boolean

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C):Either[EE,C] = for {
    a <- this; b1 <- b } yield f(a,b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]{
  override def isRight: Boolean = false

  override def isLeft: Boolean = true

}

case class Right[+A](value: A) extends Either[Nothing, A]{
  override def isRight: Boolean = true

  override def isLeft: Boolean = false

}

object Either {
  def mean(xs:IndexedSeq[Double]):Either[String,Double] =
    if(xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x:Double, y:Double):Either[Exception,Double] =
    try{
      Right( x / y )
    }catch {
      case e:Exception => Left(e)
    }
}
