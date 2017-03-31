package fpis.exercises

import scala.annotation.tailrec

object Chapter2 {
  //2.1
  def fib(n: Int): Int = {
    @tailrec
    def fib(n: Int, p: Int, a: Int): Int = if(n < 1) a else fib(n - 1, a, a + p)

    fib(n, 1, 0)
  }

  //2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val max = as.length - 1

    @tailrec
    def isSorted(index: Int): Boolean =
      if(index > max) true
      else if(ordered(as(index), as(index + 1))) isSorted(index + 1)
      else false

    isSorted(0)
  }

  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a) => f(a, _)

  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = f(_)(_)

  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =  (a) => f(g(a))
}