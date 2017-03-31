package fpis.exercises

import scala.annotation.tailrec

object Chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    //3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, tail) => tail
      case _ => throw new UnsupportedOperationException("tail of empty list")
    }

    //3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Cons(_, tail) => Cons(h, tail)
      case _ => throw new NoSuchElementException("cannot replace head of empty list")
    }

    //3.4
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _ => l
    }

    //3.5
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

    //3.6
    def init[A](l: List[A]): List[A] = l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case _ => throw new UnsupportedOperationException("init of empty list")
    }

    //3.9
    def badLength[A](as: List[A]): Int = foldRight(as, 0)((_, a) => a + 1)

    //3.10
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
      case Nil => z
    }

    //3.11
    def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

    def product(l: List[Int]) = foldLeft(l, 1)(_ * _)

    def length[A](as: List[A]): Int = foldLeft(as, 0)((a, _) => a + 1)

    //3.12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

    //3.13
    def foldRightByFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, b) => f(b, a))

    def foldRightByFoldLeft2[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    //3.14
    def append[A](a1: List[A], a2: List[A]): List[A] = foldRightByFoldLeft(a1, a2)(Cons(_, _))

    //3.15
    def concat[A](l: List[List[A]]): List[A] = foldRightByFoldLeft(l, Nil: List[A])(append)

    //3.16
    def addOne(l: List[Int]): List[Int] = foldRightByFoldLeft(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    //3.17
    def doubleToSting(l: List[Double]): List[String] = foldRightByFoldLeft(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    //3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = foldRightByFoldLeft(as, Nil: List[B])((h, t) => Cons(f(h), t))

    //3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightByFoldLeft(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

    //3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

    //3.21
    def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil)

    //3.22
    def addCorrespondingInts(xs: List[Int], ys: List[Int]): List[Int] = {
      @tailrec
      def addCorrespondingInts(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = (xs, ys) match {
        case (Cons(h1, t1), Cons(h2, t2)) => addCorrespondingInts(t1, t2, Cons(h1 + h2, acc))
        case _ => reverse(acc)
      }
      addCorrespondingInts(xs, ys, Nil: List[Int])
    }

    //3.23
    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C) = {
      @tailrec
      def zipWith(as: List[A], bs: List[B], acc: List[C]): List[C] = (as, bs) match {
        case (Cons(h1, t1), Cons(h2, t2)) => zipWith(t1, t2, Cons(f(h1, h2), acc))
        case _ => reverse(acc)
      }
      zipWith(as, bs, Nil: List[C])
    }

    //3.24
    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def matches[A](l: List[A], remainingToMatch: List[A]): Boolean = (l, remainingToMatch) match {
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => matches(t1, t2)
        case (_, Nil) => true
        case _ => false
      }
      sup match {
        case _ if(matches(sup, sub)) => true
        case Cons(_, t) => hasSubsequence(t, sub)
        case Nil => sub == Nil
      }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    //3.25
    def size[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }

    //3.26
    def max(t: Tree[Int]): Int = t match {
      case Branch(l, r) => max(l).max(max(r))
      case Leaf(i) => i
    }

    //3.27
    def depth[A](t: Tree[A]): Int = t match {
      case Branch(l, r) => 1 + depth(l).max(depth(r))
      case Leaf(i) => 0
    }

    //3.28
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(a) => Leaf(f(a))
    }

    //3.29
    def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(a) => f(a)
    }

    def sizeByFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

    def maxByFold(t: Tree[Int]): Int = fold(t)(i => i)(_ max _)

    def depthByFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(_ max _ + 1)

    def mapByFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }
}