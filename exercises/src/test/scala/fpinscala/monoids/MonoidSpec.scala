package fpinscala.monoids

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import java.util.concurrent.Executors

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

  behavior of "10.5 foldMap"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints map(_.toString)
      assert(Monoid.foldMap(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
    }
  }

  behavior of "10.6.1 foldRight"
  it should "work" in {
    val plus = (_:Int) + (_:Int)
    forAll("ints") { ints: List[Int] =>
      assert(Monoid.foldRight(ints)(0)(plus) == ints.sum)
    }
  }

  behavior of "10.6.2 foldLeft"
  it should "work" in {
    val plus = (_:Int) + (_:Int)
    forAll("ints") { ints: List[Int] =>
      assert(Monoid.foldLeft(ints)(0)(plus) == ints.sum)
    }
  }

  behavior of "10.7 foldMapV"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints.map(_.toString).toIndexedSeq
      assert(Monoid.foldMapV(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
    }
  }

  behavior of "10.8 parFoldMap"
  it should "work" in {
    import fpinscala.parallelism.Nonblocking.Par
    val es = Executors.newFixedThreadPool(4)
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints.map(_.toString).toIndexedSeq
      val parSum = Monoid.parFoldMap(intsAsStrings, Monoid.intAddition)(_.toInt)
      assert(Par.run(es)(parSum) == ints.sum)
    }
  }

  behavior of "10.9 ordered"
  it should "work" in {
    assert(Monoid.ordered(IndexedSeq()))
    assert(Monoid.ordered(IndexedSeq(1)))
    assert(Monoid.ordered(IndexedSeq(-2, 0, 1, 3, 5)))
    assert(Monoid.ordered(IndexedSeq(-2, 0, 3, 1, 6)) == false)
    forAll("ints") {ints: Seq[Int] =>
      assert(Monoid.ordered(ints.toIndexedSeq) == (ints == ints.sorted))
    }
  }

  behavior of "10.10 wcMonoid"
  it should "obey the monoid laws" in {
    import fpinscala.testing.Gen
    import fpinscala.testing.Prop.Passed
    def intGen(max: Int) = Gen.choose(0, max)
    def listGen[A](gen: Gen[A]) = gen.listOfN(intGen(10))
    val stringGen = intGen(10) flatMap(Gen.stringN)
    val stubGen = stringGen map(Monoid.Stub(_))
    val partGen = for {
      lStub <- stringGen
      words <- intGen(10)
      rStub <- stringGen
    } yield Monoid.Part(lStub, words, rStub)
    val wcGen: Gen[Monoid.WC] = Gen.union(stubGen, partGen)
    val laws = Monoid.monoidLaws(Monoid.wcMonoid, wcGen)
    assert(Monoid.run(laws, 10) == Passed)
  }

  behavior of "10.11 countWords"
  it should "work" in {
    def wordCount(s: String) = {
      def whitespaceCount = s.filter(Character.isWhitespace).size
      if (s.isEmpty) 0 else whitespaceCount + 1
    }
    forAll("s") { s: String =>
      assert(Monoid.countWords(s) == wordCount(s))
    }
  }

  val plus = (_:Int) + (_:Int)
  private def testFoldable[F[_]](foldable: Foldable[F], f: List[Int] => F[Int]) = {
    forAll("ints") { ints: List[Int] =>
      val intsF = f(ints)
      val sum = ints.sum
      assert(foldable.foldRight(intsF)(0)(plus) == sum)
      assert(foldable.foldLeft(intsF)(0)(plus) == sum)
      assert(foldable.foldMap(intsF)(_.toString)(Monoid.stringMonoid) ==
        ints.map(_.toString).fold("")(_ + _))
      assert(foldable.concatenate(intsF)(Monoid.intAddition) == sum)
      assert(foldable.toList(intsF) == ints)
    }
    
  }

  behavior of "10.12.1 ListFoldable"
  it should "work" in {
    testFoldable(ListFoldable, identity)
  }

  behavior of "10.12.2 IndexedSeqFoldable"
  it should "work" in {
    testFoldable(IndexedSeqFoldable, _.toIndexedSeq)
  }

  behavior of "10.12.3 StreamFoldable"
  it should "work" in {
    testFoldable(StreamFoldable, _.toStream)
  }
}