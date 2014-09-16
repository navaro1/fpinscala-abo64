package fpinscala.monoids

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonoidSpec extends FlatSpec with PropertyChecks {

  private def monoidLaws[A](m: Monoid[A], gen: Gen[A]) = {
    import m._
    forAll(gen label "x", gen label "y", gen label "z") { (x: A, y: A, z: A) =>
      assert(op(op(x, y), z) == op(x, op(y, z)))
    }
    forAll(gen label "x") { x: A =>
      assert(op(x, zero) == x)
      assert(op(zero, x) == x)
    }
  }

  behavior of "10.1.1 intAddition"
  it should "obey the monoid laws" in {
    monoidLaws(Monoid.intAddition, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.2 intMultiplication"
  it should "obey the monoid laws" in {
    monoidLaws(Monoid.intMultiplication, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.3 booleanOr"
  it should "obey the monoid laws" in {
    monoidLaws(Monoid.booleanOr, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.1.4 booleanAnd"
  it should "obey the monoid laws" in {
    monoidLaws(Monoid.booleanAnd, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.2 optionMonoid"
  it should "obey the monoid laws" in {
    monoidLaws(Monoid.optionMonoid[Int], Arbitrary.arbOption[Int].arbitrary)
    monoidLaws(Monoid.optionMonoid[Boolean], Arbitrary.arbOption[Boolean].arbitrary)
    monoidLaws(Monoid.optionMonoid[String], Arbitrary.arbOption[String].arbitrary)
  }
}