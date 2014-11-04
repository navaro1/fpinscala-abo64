package fpinscala.streamingio

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import SimpleStreamTransducers.{Process => SSTProcess}
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamingIOSpec extends FlatSpec with PropertyChecks {

  behavior of "15.1.1 Process.take"
  it should "work" in {
    forAll("l") { l: List[Int] =>
      forAll (Gen.chooseNum(0, l.size)) { n: Int =>
        val result = SSTProcess.take(n)(l.toStream).toList
        assert(result == l.take(n))
      }
    }
  }
}
