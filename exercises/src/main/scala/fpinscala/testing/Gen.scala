package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 {
  self =>
  def check: Boolean

  def &&(p: Prop0): Prop0 = new Prop0 {
    override def check: Boolean = self.check && p.check
  }
}

trait Prop1 {
  self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop1): Prop1 =
    new Prop1 {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = ???

  def ||(p: Prop): Prop = ???

  def tag(msg: String) = Prop { (tc, rng) =>
    run(tc, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

  // ABO
  def run(maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Result = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

  // ABO
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = ???
}

object ListProps {
  // Exercise 8.14: Prop for List.sorted
  lazy val intListGen: Gen[List[Int]] = ???
  lazy val sortedProp: Prop =
    Prop.forAll(intListGen) { l: List[Int] =>
      ???
    }

  // Exercise 8.14: Prop for List.takeWhile
  lazy val takeWhileProp: Prop = {
    val f = (_: Int) <= 0
    val p1 = Prop.forAll(intListGen) { l: List[Int] =>
      l.takeWhile(f).forall(f)
    }
    val p2: Prop = ???
    p1 && p2
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(state => {
    val (number, rng) = RNG.nonNegativeInt(state)
    val normalized = start + (number % math.abs(stopExclusive - start))
    (normalized, rng)
  }))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.nonNegativeInt)(a => a % 2 == 0)))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  // here is an example on how to combine generators in a for-comprehension
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    for {
      b <- Gen.boolean
      a <- gen
    } yield if (b) Some(a) else None

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    if (n < 1) unit(Nil)
    else
      Gen(State(state => {
        val aStates: Seq[(A, RNG)] =
          (1 until n).foldLeft(List[(A, RNG)](g.sample.run(state))) {
            case (h :: t, _) => g.sample.run(h._2) :: h :: t
          }
        aStates.headOption
          .map { case (_, rng) => (aStates.map(_._1).toList, rng) }
          .getOrElse((Nil, state))
      }))

  def stringN(n: Int): Gen[String] = ???

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](gw1: (Gen[A], Double), gw2: (Gen[A], Double)): Gen[A] = {
    val (g1, w1) = gw1
    val (g2, w2) = gw2
    val sum = w1 + w2
    val p1 = w1 / sum
    double.flatMap(d => if (d < p1) g1 else g2)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = ???

  def listOf1[A](g: Gen[A]): SGen[List[A]] = ???

  lazy val parInt: Gen[Par[Int]] = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = ???

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen.listOfN(_, this))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

  def unsized: SGen[A] = ???
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = ???

  def map[B](f: A => B): SGen[B] = ???

  def flatMap[B](f: A => SGen[B]): SGen[B] = ???

  def **[B](s2: SGen[B]): SGen[(A, B)] = ???
}
