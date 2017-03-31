package fpis.exercises

import scala.annotation.tailrec

object Chapter4 {
  sealed trait Option[+A] {
    //4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  //4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  object Option {
    //4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(f(x, _)))

    def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = map2(_, _)(f)

    //4.3
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))(map2(_, _)(_ :: _))

    //4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

    def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(o => o)
  }

  sealed trait Either[+E, +A] {
    //4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case _ => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(a => b.map(f(a, _)))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    //4.7
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E,List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))
  }
}