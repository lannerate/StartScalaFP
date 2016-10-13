package fp.datastructs

/**
  * Created by hzhang3 on 10/13/2016.
  */

sealed trait Option[+A] {
  def isEmpty: Boolean

  def get: A

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]):Option[B]
}

case class Some[+A](x: A) extends Option[A] {
  override def isEmpty: Boolean = false

  override def get = x

  override def map[B](f: A => B): Option[B] = Some(f(x))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(x)
}

case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true

  override def get = throw new NoSuchElementException("None have not value!")

  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
}

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def apply[A](x: A):Option[A] = if (x == null) None else Some(x)
}