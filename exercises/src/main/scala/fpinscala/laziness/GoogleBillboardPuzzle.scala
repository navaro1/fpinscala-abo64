package fpinscala.laziness

import java.math.MathContext
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

object GoogleBillboardPuzzle extends App {

  // Kestrel combinator: perform some side effect before returning result
  private def kestrel[A](x: A)(f: A => Unit): A = { f(x); x }

  private def timed[A](name: String)(block: => A): A = {
    val start = System.currentTimeMillis
    kestrel(block) { value =>
      val duration = Duration(System.currentTimeMillis -start, TimeUnit.MILLISECONDS)
      println(s"$name: $value [$duration]")
    }
  }

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

  def isPrime(i: Long) : Boolean = {
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

  def decimalsAsString(n: BigDecimal) = {
    val str = n.toString
    val decPos = str.indexWhere(_ == '.')
    if (decPos > -1) str.substring(decPos + 1) else ""
  }

  def findStreamSolution(digits: Int) = {
    var n = 0
    def moreIterations = kestrel(n < EulerIterations)(_ => n += 1) // make sure that precision is high enough
    val solutionStream = eulerStream map decimalsAsString map(findFirstPrime(_, digits))
    solutionStream.dropWhile(moreIterations || _.isEmpty).headOption.get
  }

  timed("Euler w/ loop  ")(calculateEuler(EulerIterations))
  timed("Euler w/ Stream")(eulerStream.drop(EulerIterations).headOption.get)

  val ExpectedSolution = 7427466391L
  def checkSolution(solution: Option[String]): Unit =
    assert(solution.get.toLong == ExpectedSolution)

  val solution = timed("solution w/ findFirstPrime"){
    findFirstPrime(decimalsAsString(calculateEuler(EulerIterations)), 10)
  }
  checkSolution(solution)

  val streamSolution = timed("solution w/ mapped Stream "){
    findStreamSolution(10)
  }
  checkSolution(streamSolution)
}