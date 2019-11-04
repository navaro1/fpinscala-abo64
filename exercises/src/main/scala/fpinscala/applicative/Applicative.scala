package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._

trait Applicative[F[_]] extends Functor[F] {
  self =>

  implicit class ApplicativeOps[A](fa: F[A]) {
    def apply[B](fab: F[A => B]): F[B] = self.apply(fab)(fa)

    def map[B](f: A => B): F[B] = self.map(fa)(f)
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil     => unit(List())
    case h :: tl => map2(h, sequence(tl))(_ :: _)
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as map f)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(map2(fa, fb)((a, b) => f.curried(a)(b)))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
    f: (A, B, C, D) => E
  ): F[E] =
    apply(map3(fa, fb, fc)((a, b, c) => f.curried(a)(b)(c)))(fd)

  def factor[A, B](fa: F[A], fb: F[A]): F[(A, B)] = ???

  def product[G[_]](
    G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self: Applicative[F] = this
    new Applicative[
      ({
        type f[x] = (F[x], G[x])
      })#f
    ] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](
        fab: (F[A => B], G[A => B])
      )(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](
    G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self: Applicative[F] = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]],
                                 fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(this.unit(Map.empty[K, V]))(
      (facc, fa) => this.map2(fa._2, facc)((a, acc) => acc + (fa._1 -> a))
    )
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[
      ({
        type f[x] = Either[E, x]
      })#f
    ] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def join[A](mma: Either[E, Either[E, A]]): Either[E, A] =
        mma match {
          case Left(e)   => Left(e)
          case Right(ma) => ma
        }

      override def map[A, B](ma: Either[E, A])(f: A => B): Either[E, B] =
        ma match {
          case Left(e)  => Left(e)
          case Right(a) => Right(f(a))
        }
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def map2[A, B, C](fa: State[S, A],
                               fb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  def composeM[F[_], N[_]](implicit F: Monad[F],
                           N: Monad[N],
                           T: Traverse[N]): Monad[({ type f[x] = F[N[x]] })#f] =
    ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  lazy val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A],
                               fb: List[B])(f: (A, B) => C): List[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  lazy val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A],
                               fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A],
                               b: Stream[B])( // Combine elements pointwise
                                             f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]
    : Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[
      ({
        type f[x] = Validation[E, x]
      })#f
    ] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
        f: (A, B) => C
      ): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b))          => Success(f(a, b))
        case (Success(_), Failure(head, tail)) => Failure(head, tail)
        case (Failure(head, tail), Success(_)) => Failure(head, tail)
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, t1 ++ Vector(h2) ++ t2)
      }

    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)

      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(
        f: (A, B) => C
      ): Const[M, C] = ???
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
      ???
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(
      (a: A) =>
        (for {
          s1 <- get[S]
          (b, s2) = f(a, s1)
          _ <- set(s2)
        } yield b)
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(
    implicit G: Applicative[G],
    H: Applicative[H]
  ): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](
    implicit G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  lazy val listTraverse: Traverse[List] = new Traverse[List] {}

  lazy val optionTraverse: Traverse[Option] = new Traverse[Option] {}

  lazy val treeTraverse: Traverse[Tree] = new Traverse[Tree] {}
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
