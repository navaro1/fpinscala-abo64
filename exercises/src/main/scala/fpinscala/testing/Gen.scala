package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 { self =>
  def check: Boolean
  def &&(p: Prop0): Prop0 =
    new Prop0 { override def check = self.check && p.check }
}

trait Prop { self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop =
    new Prop {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
//    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
    def run(rng: RNG): (Int, RNG) = {
      val (i, r) = RNG.nonNegativeInt(rng)
      val result = (i % math.abs(stopExclusive - start)) + start
      (result, r)
    }
    val sample = State[RNG, Int](run)
//    def loop(r: RNG): (Int, RNG) = {
//      val (i, r2) = r.nextInt
//      if (i >= start && i < stopExclusive) (i, r2) else loop(r2)
//    }
//    val sample = State[RNG, Int](loop)
    Gen[Int](sample)
  }

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map((_:List[Int]).map(_.toChar).mkString)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean flatMap {b => if (b) g1 else g2}

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen.double flatMap {d => if (d < g1Threshold) g1._1 else g2._1}
  }
}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen.listOfN(_, this))
}

trait SGen[+A] {

}

