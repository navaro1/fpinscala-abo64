package fpinscala.monoids

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonoidSpec extends FlatSpec with PropertyChecks {

  private def checkMonoidLaws[A](m: Monoid[A], gen: Gen[A],
      isEqual: (A,A) => Boolean = (_:A) == (_:A)) =
  {
    def assertEquals(a1: A, a2: A) = assert(isEqual(a1, a2), s"$a1 == $a2")
    import m._
    forAll(gen label "x", gen label "y", gen label "z") { (x: A, y: A, z: A) =>
      assertEquals(op(op(x, y), z), op(x, op(y, z)))
    }
    forAll(gen label "x") { x: A =>
      assertEquals(op(x, zero), x)
      assertEquals(op(zero, x), x)
    }
  }

  behavior of "10.1.1 intAddition"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.intAddition, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.2 intMultiplication"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.intMultiplication, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.3 booleanOr"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.booleanOr, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.1.4 booleanAnd"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.booleanAnd, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.2 optionMonoid"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.optionMonoid[Int], Arbitrary.arbOption[Int].arbitrary)
    checkMonoidLaws(Monoid.optionMonoid[Boolean], Arbitrary.arbOption[Boolean].arbitrary)
    checkMonoidLaws(Monoid.optionMonoid[String], Arbitrary.arbOption[String].arbitrary)
  }

  behavior of "10.3 endoMonoid"
  it should "obey the monoid laws" in {
    def isEqual[A](f: A => A, g: A => A)(implicit arb: Arbitrary[A]): Boolean = {
      forAll("a") { a: A => assert(f(a) == g(a))}
      true
    }
    val booleanFunctionGen =
      Gen.oneOf[Boolean => Boolean]({x: Boolean => !x}, identity[Boolean] _)
    checkMonoidLaws(Monoid.endoMonoid[Boolean], booleanFunctionGen, isEqual[Boolean])
  }

  behavior of "10.4 monoidLaws"
  it should "check all written monoids so far" in {
    import fpinscala.testing.Prop.Passed
    assert(Monoid.testMonoidLaws(10) == Passed)
  }

}