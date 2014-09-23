package fpinscala.laziness

import java.math.MathContext

object GoogleBillboardPuzzle extends App {

  private def kestrel[A](x: A)(f: A => Unit): A = { f(x); x }

  val precision = new MathContext(10000)
  val zero = BigDecimal(0, precision)
  val one = BigDecimal(1, precision)

  def inverse(n: BigDecimal): BigDecimal = //kestrel(one / n)(_ => println(n))
    one / n

  def factorial(n: Int): BigDecimal =
    if (n == 0) one else (1 to n).foldLeft(one)(_ * _)

  def addEulerSeriesElement(b: BigDecimal, i: Int): BigDecimal =
    b + inverse(factorial(i))

  def euler(n: Int): BigDecimal = {
    (0 to n).toSeq.foldLeft(zero)(addEulerSeriesElement)
  }

  /** Stream of BigDecimals approaching Euler's number */
  def eulerStream: Stream[BigDecimal] =
    Stream.unfold((0, one)) {
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

  val eulerNumber: BigDecimal = //euler(100)
    eulerStream.drop(100).headOption.get
//  println(eulerNumber)

  def findFirstPrime(str: String, digits: Int): Option[String] = {
    str.sliding(digits).find(s => isPrime(s.toLong))
  }

  def decimalsAsString(n: BigDecimal) = {
    val str = n.toString
    val decPos = str.indexWhere(_ == '.')
    if (decPos > -1) str.substring(decPos + 1) else ""
  }

  val solution = findFirstPrime(decimalsAsString(eulerNumber), 10)
  println(s"solution: $solution")
  assert(solution.get.toLong == 7427466391L)
  // solution: 7427466391
}