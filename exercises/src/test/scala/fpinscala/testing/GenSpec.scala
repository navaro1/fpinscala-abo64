package fpinscala.testing

import org.junit.runner.RunWith
import org.scalacheck.{Gen => SCGen}
import org.scalacheck.{Prop => SCProp}
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import fpinscala.state.RNG

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class GenSpec extends FlatSpec with PropertyChecks {

  behavior of "8.1 List.sum"

  it should "obey some laws" in {
    val ints = SCGen.choose(0, 100)
    val intList = SCGen.listOf(ints)
    val prop =
      SCProp.forAll(intList) {l => l.sum == l.reverse.sum} &&
      SCProp.forAll(ints, ints) {(n, i) => List.fill(n)(i).sum == n * i}
    prop.check
  }

  behavior of "8.2 List.max"

  it should "obey some laws" in {
    val ints = SCGen.choose(0, 100)
    val intList = SCGen.listOf1(ints)
    val prop =
      SCProp.forAll(intList) {l => l.max == l.reverse.max} &&
      SCProp.forAll(ints map(_ + 1), ints) {(n, i) => List.fill(n)(i).max == i}
    prop.check
  }

  behavior of "8.3 Prop.&&"

  it should "work" in {
    def asProp(b: Boolean): Prop0 = new Prop0 {override def check = b}
    def testAnd[A,B](check1: Boolean, check2: Boolean, expected: Boolean) =
      assertResult(expected)((asProp(check1) && asProp(check2)).check)

    val tests = Table(
      ("prop1.check", "prop2.check", "(prop1 && prop2).check"),
      (true, true, true),
      (true, false, false),
      (false, true, false),
      (false, false, false)
    )
    forAll(tests)(testAnd)
  }

  behavior of "8.4 choose"

  it should "should stay within specified range" in {
    var rng: RNG = RNG.Simple(0)
    val startInts = SCGen.choose(-100, 100)
    forAll(startInts label "start") { start =>
      forAll(SCGen.choose(start, start + 100) label "stopExclusive") { stopExclusive =>
        val (i, rng1) = Gen.choose(start, stopExclusive).sample.run(rng)
        rng = rng1
        println(s"start=$start,stopExclusive=$stopExclusive,i=$i")
        assert(i >= start && i < stopExclusive)//, s"$i >= $start && $i < $stopExclusive")
      }
    }
  }
}
