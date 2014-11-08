package fpinscala.streamingio

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import SimpleStreamTransducers.{Process => Process1}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamingIOSpec extends FlatSpec with PropertyChecks {

  private def between0AndN(n: Int) = Gen.chooseNum(0, n) label "n"

  behavior of "15.1.1 Process1.take"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = Process1.take(n)(l.toStream)
        assert(result.toList == l.take(n))
      }
    }
  }

  behavior of "15.1.2 Process1.drop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = Process1.drop(n)(l.toStream)
        assert(result.toList == l.drop(n))
      }
    }
  }

  private def even(i: Int) = (i % 2) == 0

  behavior of "15.1.3 Process1.takeWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.takeWhile(even)(l.toStream)
      assert(result.toList == l.takeWhile(even))
    }
  }

  behavior of "15.1.4 Process1.dropWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.dropWhile(even)(l.toStream)
      assert(result.toList == l.dropWhile(even))
    }
  }

  behavior of "15.2 Process1.count"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.count(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }

  private def listOfMeans(doubles: List[Double]) = {
    val (reversedMeans, _, _) = doubles.foldLeft((List[Double](), 0d, 0)) {
      case ((means, sum, index), d) =>
        (((sum + d) / (index + 1)) :: means, (sum + d), index + 1)
    }
    reversedMeans.reverse
  }

  behavior of "15.3 Process1.mean"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = Process1.mean(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

  behavior of "15.4.1 Process1.sumViaLoop"
  it should "work" in {
    def listOfSums(doubles: List[Double]) = {
      val (reversedSums, _) = doubles.foldLeft((List[Double](), 0d)) {
        case ((sums, sum), d) => ((sum + d) :: sums, sum + d)
      }
      reversedSums.reverse
    }
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = Process1.sumViaLoop(doubles.toStream)
      assert(result.toList == listOfSums(doubles))
    }
  }

  behavior of "15.4.2 Process1.countViaLoop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.countViaLoop(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }

  behavior of "15.5 Process1.|>"
  it should "work" in {
    val evenProcess = Process1.filter(even)
    val toStringProcess = Process1.lift((_:Int).toString)
    val pipe = evenProcess |> toStringProcess
    forAll("l") { l: List[Int] =>
      val result = pipe(l.toStream)
      assert(result.toList == l.filter(even).map(_.toString))
    }
  }

  behavior of "15.6 Process1.zipWithIndex"
  it should "work in object Process" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.zipWithIndex(Process1.id[Int])(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }
  it should "work in trait Process" in {
    forAll("l") { l: List[Int] =>
      val result = Process1.id[Int].zipWithIndex(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }

  behavior of "15.7.1 Process1.zip"
  it should "work" in {
    val plus1 = (_:Int) + 1
    val plus1Process = Process1.lift(plus1)
    forAll("l") { l: List[Int] =>
      val result = Process1.zip(Process1.id[Int], plus1Process)(l.toStream)
      assert(result.toList == l.zip(l map plus1))
    }
  }

  behavior of "15.7.2 Process1.meanViaZip"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = Process1.meanViaZip(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

  behavior of "15.8 Process1.exists"
  it should "work" in {
    val isZero = (_:Int) == 0
    def existsResult(l: List[Int]): List[Boolean] =
      l.foldLeft((List[Boolean](),false)) { case ((lb,foundZero), i) =>
        ((foundZero || isZero(i))::lb, foundZero || isZero(i))
      }._1.reverse
    forAll("l") { l: List[Int] =>
      val result = Process1.exists(isZero)(l.toStream)
      assert(result.toList == existsResult(l))
    }
  }

  behavior of "15.9 Process1.convertFahrenheit"
  it should "work" in {
    implicit val arbLine: Arbitrary[String] = {
      val whitespace = Gen.oneOf(" ", "\t")
      val fahrenheit = Gen.chooseNum(-100d, 200d)
      val blankLine = Gen.listOf(whitespace).map(_.mkString)
      val commentLine = Gen.const("# no comment!")
      val celsiusLine = fahrenheit map(_.toString)
      Arbitrary(Gen.frequency((3,celsiusLine), (1,commentLine), (1,blankLine)))
    }
    forAll("line") { lines: List[String] =>
      val result = Process1.convertFahrenheit(lines.toStream)
      val celsiusLines =
        lines.filter(!_.trim.isEmpty)
             .filter(!_.startsWith("#"))
             .map(line => Process1.toCelsius(line.trim.toDouble).toString)
      assert(result.toList == celsiusLines)
    }
  }
}
