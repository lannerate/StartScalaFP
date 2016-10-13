package fp.datastructs

/**
  * Created by hzhang3 on 10/13/2016.
  */

sealed trait Option[+A]

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs:Seq[Double]):Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)


}