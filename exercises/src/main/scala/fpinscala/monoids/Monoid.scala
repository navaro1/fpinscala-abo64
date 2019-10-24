package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import scala.collection.immutable.PagedSeq
import scala.collection.{immutable, mutable} // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  lazy val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = Option.empty
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A =
      a1 andThen a2

    override def zero: A => A = identity
  }

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    import m._
    val withZero = Gen.union(gen, Gen.unit(zero)) // zero as interesting corner case
    // Associativity
    forAll(for {
      x <- withZero
      y <- withZero
      z <- withZero
    } yield (x, y, z)) { case (x, y, z) =>
      op(x, op(y, z)) == op(op(x, y), z)
    } &&
      // Identity
      forAll(gen)(a =>
        op(a, zero) == a && op(zero, a) == a)
  }


  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => (b: B) => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case xs if xs.isEmpty => m.zero
    case h1 +: xs if xs.isEmpty => f(h1)
    case xs =>
      val (l, r) = xs.splitAt(xs.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(ints.toList, new Monoid[(Int, Boolean)] {
      override def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) =
        (a2._1, a1._2 && a2._2 && a1._1 <= a2._1)

      override def zero: (Int, Boolean) = (Int.MinValue, true)
    })(e => (e, true))._2

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.lazyUnit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.lazyUnit(_).map(f))

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(
          l1,
          w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1),
          r2
        )
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
    }

    override def zero: WC = Stub("")
  }

  def countWords(s: String): Int = {
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B =
        a => B.op(a1(a), a2(a))

      override def zero: A => B = _ => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List[A]())((acc, e) => e :: acc)
}

object ListFoldable extends Foldable[List] {
  //  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
  //
  //
  //  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
  //    f

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  //  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
  //    sys.error("todo")
  //
  //  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
  //    sys.error("todo")

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  //  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
  //    sys.error("todo")
  //
  //  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
  //    sys.error("todo")

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case h #:: hs => mb.op(f(h), foldMap(hs)(f)(mb))
    case Stream.Empty => mb.zero
  }


}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  //  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
  //    sys.error("todo")
  //
  //  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
  //    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(x) => f(x)
    case None => mb.zero
  }

  //  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
  //    sys.error("todo")
  //
  //  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
  //    sys.error("todo")
}

