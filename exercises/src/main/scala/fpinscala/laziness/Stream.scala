package fpinscala.laziness

import fpinscala.laziness.Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (_, Empty) => Empty
    case (m, _) if m < 1 => Empty
    case (m, Cons(h, t)) => Cons(h, () => t().take(m - 1))
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Empty, _) => Option.empty
      case (_, m) if m < 1 => Option.empty
      case (Cons(h, t), m) => Some((h(), (t(), m - 1)))
    }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (_, Empty) => Empty
    case (m, t) if m < 1 => t
    case (m, Cons(_, t)) => t().drop(m - 1)
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(a, as) if p(a()) => cons(a(), as().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => Option.empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)(e => e match {
      case Empty => Option.empty[(B,Stream[A])]
      case Cons(h, t) => Some((f(h()), t()))
    })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](other: => Stream[B]): Stream[B] =
    this.foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = sys.error("todo")

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def tails: Stream[Stream[A]] = sys.error("todo using unfold")

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] = sys.error("todo")
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
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  lazy val fibs: Stream[Int] = {
    def loop(n0: Int, n1: Int): Stream[Int] =
      cons(n0, loop(n1, n0 + n1))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map {case (a, s) => cons(a, unfold(s)(f))}.getOrElse(Empty)

  lazy val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)){case (n0, n1) => Some((n0, (n1, n0 + n1)))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n){ case s => Some((s, s + 1))}

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(e => Some((e, e)))

  lazy val onesViaUnfold: Stream[Int] = constantViaUnfold(1)
}