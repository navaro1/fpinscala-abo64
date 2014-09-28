package fpinscala.laziness

import java.math.MathContext
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.math.BigDecimal.int2bigDecimal

/**
 * Solutions with Seq.fold and Stream.unfold for this problem:
 * http://mathworld.wolfram.com/news/2004-10-13/google/
 * http://google-tale.blogspot.de/2008/07/google-billboard-puzzle.html
 */
object GoogleBillboardPuzzle extends App {

  // Kestrel combinator: perform some side effect before returning result
  // http://stackoverflow.com/questions/9671620/how-to-keep-return-value-when-logging-in-scala/9673294#9673294
  // http://debasishg.blogspot.de/2009/09/side-effects-with-kestrel-in-scala.html
  private def kestrel[A](x: A)(f: A => Unit): A = { f(x); x }

  private def timed[A](name: String)(block: => A): A = {
    val start = System.currentTimeMillis
    kestrel(block) { value =>
      val duration = Duration(System.currentTimeMillis - start, TimeUnit.MILLISECONDS)
      println(s"$name: $value [$duration]")
    }
  }

  object EulerCalculator {
    // # of iterations required to get good enough precision of Euler's number
    val EulerIterations = 75

    val DecimalPrecision = new MathContext(10000)
    val Zero = BigDecimal(0, DecimalPrecision)
    val One = BigDecimal(1, DecimalPrecision)

    def inverse(n: BigDecimal): BigDecimal = //kestrel(One / n)(_ => println(n))
      One / n

    def factorial(n: Int): BigDecimal =
      if (n == 0) One else (1 to n).foldLeft(One)(_ * _)

    def addEulerSeriesElement(b: BigDecimal, i: Int): BigDecimal =
      b + inverse(factorial(i))

    def calculateEuler(n: Int): BigDecimal = {
      (0 to n).toSeq.foldLeft(Zero)(addEulerSeriesElement)
    }

    /** Stream of BigDecimals approaching Euler's number */
    def eulerStream: Stream[BigDecimal] =
      Stream.unfold((0, Zero)) {
        case (n, e) => {
          val euler = addEulerSeriesElement(e, n)
          Some((euler, (n + 1, euler)))
        }
      }

    // make sure that decimal precision is high enough
    def preciseEulerStream = eulerStream.drop(EulerIterations)
  }

  object PrimeFinder {
    def isPrime(i: Long): Boolean = {
      if (i <= 1) false
      else if (i == 2) true
      else if (i % 2 == 0) false
      else !(3L to math.sqrt(i).toLong by 2).exists(i % _ == 0)
    }
    //(0 to 100) foreach { x => println(s"$x: ${isPrime(x)}")}

    def findFirstPrime(str: String, digits: Int): Option[String] = {
      str.sliding(digits).find(s =>
        s.length == digits && //kestrel(isPrime(s.toLong))(b => if (b) println(s)))
        isPrime(s.toLong))
    }

    def findStreamSolution(primeDigits: Int): Option[String] = {
      import EulerCalculator._
      val solutionStream: Stream[Option[String]] =
        preciseEulerStream map decimalsAsString map (findFirstPrime(_, primeDigits))
      solutionStream.headOption.get
    }

    def decimalsAsString(n: BigDecimal): String = {
      val str = n.toString
      val decPos = str.indexWhere(_ == '.')
      if (decPos > -1) str.substring(decPos + 1) else ""
    }
  }

  import EulerCalculator._
  import PrimeFinder._

  timed("  Euler w/ fold")(calculateEuler(EulerIterations))
  timed("Euler w/ unfold")(preciseEulerStream.headOption.get)

  val ExpectedSolution = 7427466391L
  def checkSolution(solution: Option[String]): Unit =
    assert(solution.get.toLong == ExpectedSolution)

  val solution = timed("solution w/ findFirstPrime"){
    findFirstPrime(decimalsAsString(calculateEuler(EulerIterations)), 10)
  }
  checkSolution(solution)

  val streamSolution = timed(" solution w/ mapped Stream"){
    findStreamSolution(10)
  }
  checkSolution(streamSolution)
}