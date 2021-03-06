package fp.datastructs

import java.util.regex.{Pattern, PatternSyntaxException}

/**
  * Created by hzhang3 on 10/13/2016.
  */

sealed trait Option[+A] {
  def isEmpty: Boolean

  def get: A

  def map[B](f: A => B): Option[B] = if (isEmpty) None else Some(f(get))

  def flatMap[B](f: A => Option[B]):Option[B] = if (isEmpty) None else f(get)

  def getOrElse[B >:A](default: => B): B = if(isEmpty) default else get

  def orElse[B >:A](ob : =>Option[B]): Option[B] = if(isEmpty) ob else Some(get)

  def filter(f: A => Boolean): Option[A] = if(f(get)) Some(get) else None

  def lift[A,B](f: A => B):Option[A]=>Option[B] = _ map f
}

case class Some[+A](x: A) extends Option[A] {
  override def isEmpty: Boolean = false

  override def get = x
}

case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true

  override def get = throw new NoSuchElementException("None have not value!")
}

object Options {

  def apply[A](x: A):Option[A] = if (x == null) None else Some(x)

  def pattern(s:String):Option[Pattern] =
    try{
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(regex:String):Option[String => Boolean] =
    pattern(regex) map( reg => (s:String) => reg.matcher(s).matches() )

  def mkMatcher_1(reg:String):Option[String => Boolean] =
    for {
      p <- pattern(reg)
    }yield ((s:String) => p.matcher(s).matches())

  def doesMatches(reg:String,s:String):Option[Boolean] =
    for {
      p <- mkMatcher_1(reg)
    }yield p(s)

  def bothMatch(reg1:String,reg2:String,s:String):Option[Boolean]=
    for {
      f <- mkMatcher(reg1)
      g <- mkMatcher(reg2)
    }yield f(s) && g(s)

  def bothMatch_1(reg1:String, reg2:String, s:String):Option[Boolean] =
    mkMatcher(reg1) flatMap( f => mkMatcher(reg2) map( g => f(s) && g(s) ) )

  def map2[A,B,C](a:Option[A], b:Option[B])(f : (A,B) => C):Option[C] =
    a flatMap( aa => b map( bb => f(aa,bb)))

  def bothMatch_2(reg1:String, reg2:String, s:String):Option[Boolean] =
    map2(mkMatcher(reg1),mkMatcher(reg2))( (a,b) => a.apply(s) && b.apply(s) )

  def sequence[A](a: scala.List[Option[A]]):Option[scala.List[A]] = a match {
    case scala.Nil => Some(scala.Nil)
    case h :: t => h.flatMap( hh => sequence(t) map( hh :: _ ))
  }

  def parsePattern(a:scala.List[String]):Option[scala.List[Pattern]]=
    sequence( a map pattern )

  def traverse_2[A,B](a:scala.List[A])(f: A => Option[B]):Option[scala.List[B]] =
    sequence( a map f)

  def traverse[A,B](a:scala.List[A])(f: A => Option[B]):Option[scala.List[B]] = a match {
    case scala.Nil => Some(scala.Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceByTraverse[A](a: scala.List[Option[A]]):Option[scala.List[A]] =
    traverse(a)(t => t)
}

object Math {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs) flatMap (m => mean( xs.map( x => math.pow(x-m,2) ) ))

}