package fp.datastructs

import scala.runtime.Nothing$
import scala.util.hashing.Hashing.Default

/**
  * Created by hzhang3 on 10/13/2016.
  */

sealed trait Option[+A] {
  def isEmpty: Boolean

  def get: A

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]):Option[B]

  def getOrElse[B >:A](default: => B): B

  def orElse[B >:A](ob : =>Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](x: A) extends Option[A] {
  override def isEmpty: Boolean = false

  override def get = x

  override def map[B](f: A => B): Option[B] = Some(f(x))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(x)

  override def getOrElse[B >: A](default: => B): B = if(x == null) default else x

  override def orElse[B >: A](ob: => Option[B]): Option[B] = if (x == null) ob else Some(x)

  override def filter(f: (A) => Boolean): Option[A] = if (f(x)) Some(x) else None
}

case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true

  override def get = throw new NoSuchElementException("None have not value!")

  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
}

object Option {


  def apply[A](x: A):Option[A] = if (x == null) None else Some(x)

}

object Math {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs) flatMap (m => mean( xs.map( x => math.pow(x-m,2) ) ))

}