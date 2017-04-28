package fpis.exercises

import scala.annotation.tailrec

object Chapter5 {
  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    //5.1
    def toList: List[A] = {
      @tailrec
      def toList(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => toList(t(), h() :: acc)
        case Empty => acc.reverse
      }
      toList(this, Nil)
    }

    //5.2
    final def take(n: Int): Stream[A] = this match {
      case Cons(h, t) => if(n < 1) Empty else Stream.cons(h(), t().take(n - 1))
      case Empty => Empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n < 1 => t().drop(n - 1)
      case _ => this
    }

    //5.3
    final def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) =>
        val head = h()
        if(f(head)) Stream.cons(head, t().takeWhile(f)) else Empty
      case Empty => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    //5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    //5.5
    def takeWhile2(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h,t) => if(f(h)) cons(h, t) else Empty)

    //5.6
    def headOption2: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

    //5.7
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((h,t) => if (f(h)) cons(h, t) else t)

    def append[B>:A](sa: => Stream[B]): Stream[B] = foldRight(sa)((h,t) => cons(h,t))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((h,t) => f(h) append t)

    //5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //5.13
    def mapUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

    def takeWhileUnfold(f: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case _ => None
    }

    //5.14
    def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll{case (a, b) => a == b}

    //5.15
    def tails: Stream[Stream[A]] = cons(this, unfold(this){
      case Cons(_, h2) => Some(h2() -> h2())
      case Empty => None
    })

    //5.16
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z))((a, acc) => cons(f(a, acc.headOption.getOrElse(z)), acc))
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    //5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //5.10
    val fib: Stream[Int] = {
      def addLast2(x: Int, y: Int): Stream[Int] = Cons(() => x, () => addLast2(y, x + y))
      addLast2(0, 1)
    }

    //5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).fold(empty[A]){case (a, s) => cons(a, unfold(s)(f))}

    //5.12
    val fibsUnfold: Stream[Int] = unfold((0,1)){case (x, y) => Some((x, (y, x + y)))}

    def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

    def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))
  }


}
