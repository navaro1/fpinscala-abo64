package fpinscala.streamingio

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import SimpleStreamTransducers.{Process => SSTProcess}
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamingIOSpec extends FlatSpec with PropertyChecks {

  private def between0AndN(n: Int) = Gen.chooseNum(0, n) label "n"

  behavior of "15.1.1 Process.take"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = SSTProcess.take(n)(l.toStream)
        assert(result.toList == l.take(n))
      }
    }
  }

  behavior of "15.1.2 Process.drop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (between0AndN(l.size)) { n: Int =>
        val result = SSTProcess.drop(n)(l.toStream)
        assert(result.toList == l.drop(n))
      }
    }
  }

  private def even(i: Int) = (i % 2) == 0

  behavior of "15.1.3 Process.takeWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.takeWhile(even)(l.toStream)
      assert(result.toList == l.takeWhile(even))
    }
  }

  behavior of "15.1.4 Process.dropWhile"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.dropWhile(even)(l.toStream)
      assert(result.toList == l.dropWhile(even))
    }
  }

  behavior of "15.2 Process.count"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.count(l.toStream)
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

  behavior of "15.3 Process.mean"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SSTProcess.mean(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

  behavior of "15.4.1 Process.sumViaLoop"
  it should "work" in {
    def listOfSums(doubles: List[Double]) = {
      val (reversedSums, _) = doubles.foldLeft((List[Double](), 0d)) {
        case ((sums, sum), d) => ((sum + d) :: sums, sum + d)
      }
      reversedSums.reverse
    }
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SSTProcess.sumViaLoop(doubles.toStream)
      assert(result.toList == listOfSums(doubles))
    }
  }

  behavior of "15.4.2 Process.countViaLoop"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.countViaLoop(l.toStream)
      assert(result.toList == l.zipWithIndex.map(_._2 + 1))
    }
  }

  behavior of "15.5 Process.|>"
  it should "work" in {
    val evenProcess = SSTProcess.filter(even)
    val toStringProcess = SSTProcess.lift((_:Int).toString)
    val pipe = evenProcess |> toStringProcess
    forAll("l") { l: List[Int] =>
      val result = pipe(l.toStream)
      assert(result.toList == l.filter(even).map(_.toString))
    }
  }

  behavior of "15.6 Process.zipWithIndex"
  it should "work in object Process" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.zipWithIndex(SSTProcess.id[Int])(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }
  it should "work in trait Process" in {
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.id[Int].zipWithIndex(l.toStream)
      assert(result.toList == l.zipWithIndex)
    }
  }

  behavior of "15.7.1 Process.zip"
  it should "work" in {
    val plus1 = (_:Int) + 1
    val plus1Process = SSTProcess.lift(plus1)
    forAll("l") { l: List[Int] =>
      val result = SSTProcess.zip(SSTProcess.id[Int], plus1Process)(l.toStream)
      assert(result.toList == l.zip(l map plus1))
    }
  }

  behavior of "15.7.2 Process.meanViaZip"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      val doubles = l.map(_.toDouble)
      val result = SSTProcess.meanViaZip(doubles.toStream)
      assert(result.toList == listOfMeans(doubles))
    }
  }

}
