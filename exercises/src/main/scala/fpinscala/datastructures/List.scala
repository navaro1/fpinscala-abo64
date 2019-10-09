package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((id, e) => b => id(f(e, b)))(z)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Empty list")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("Empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else if (l == Nil) l
    else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("emtpy list")
    case Cons(_, Nil) => Nil
    case Cons(h1, Cons(_, Nil)) => Cons(h1, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumViaFoldLeft(nums: List[Int]): Int = foldLeft(nums, 0)(_ + _)

  def productViaFoldLeft(nums: List[Double]): Double = foldLeft(nums, 1.0)(_ * _)

  def lengthViaFoldLeft(l: List[_]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((e, acc) => Cons(e, acc))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, (b: List[A]) => b)((g, e) => b => g(Cons(e, b)))(a2)

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((acc, e) => appendViaFoldLeft(acc, e))

  def add1(nums: List[Int]): List[Int] =
    foldRightViaFoldLeft(nums, List[Int]())((e, acc) => Cons(e + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, List[String]())((e, acc) => Cons(e.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, List[B]())((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, List[A]())((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(l, List[B]())((e, acc) => appendViaFoldLeft(f(e), acc))
  // concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(e => if (f(e)) List(e) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = sys.error("todo")
}
