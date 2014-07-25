package fpinscala.laziness

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Stream._
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class StreamSpec extends FlatSpec with PropertyChecks {

  implicit def arbStream[T](implicit ev: Arbitrary[Seq[T]]): Arbitrary[Stream[T]] =
    Arbitrary(arbitrary[Seq[T]] map (Stream(_: _*)))

  implicit def arbStreamListTuple[T](implicit ev: Arbitrary[List[T]]): Arbitrary[(Stream[T],List[T])] =
    Arbitrary(arbitrary[List[T]] map { l => (Stream(l: _*), l) })

  behavior of "5.1 toList"

  it should "work" in {
    def testToList[A](as: Stream[A], expected: List[A]) = assertResult(expected)(as.toList)

    val tests = Table(
      ("as", "as.toList"),
      (Stream(), Nil),
      (Stream(0), List(0)),
      (Stream("a", "b"), List("a", "b")))
    forAll(tests)(testToList)
  }

  it should "for all l: List[T] ==> Stream[T](l).toList == l" in {
    forAll("l: List[_]") { l: List[Int] =>
      assertResult(l)(Stream(l: _*).toList)
    }
  }

  behavior of "5.2.1 take"

  it should "work" in {
    def testTake[A](as: Stream[A], n: Int, expected: List[A]) = assertResult(expected)(as.take(n).toList)

    val tests = Table(
      ("as: Stream[_]", "n", "as.take(n)"),
      (Stream(), 1, Nil),
      (Stream(0, 1, 2), 2, List(0, 1)),
      (Stream("a", "b"), 3, List("a", "b")))
    forAll(tests)(testTake)
  }

  it should "for all l: List[_], n: Int ==> Stream(l).take(n).toList == l.take(n)" in {
    forAll("l: List[_]") { l: List[Int] =>
      val size = l.size
      forAll (Gen.chooseNum(0, size)) { n: Int =>
        assertResult(l.take(n))(Stream(l: _*).take(n).toList)
      }
    }
  }

  behavior of "5.2.2 drop"

  it should "work" in {
    def testDrop[A](as: Stream[A], n: Int, expected: List[A]) = assertResult(expected)(as.drop(n).toList)

    val tests = Table(
      ("as: Stream[_]", "n", "as.take(n)"),
      (Stream(), 1, Nil),
      (Stream(0, 1, 2), 2, List(2)),
      (Stream("a", "b"), 2, Nil),
      (Stream("a", "b"), 3, Nil))
    forAll(tests)(testDrop)
  }

  it should "for all l: List[_], n: Int ==> Stream(l).drop(n).toList == l.drop(n)" in {
    forAll("l: List[_]") { l: List[Int] =>
      val size = l.size
      forAll (Gen.chooseNum(0, size)) { n: Int =>
        assertResult(l.drop(n))(Stream(l: _*).drop(n).toList)
      }
    }
  }

  behavior of "5.3 takeWhile"

  def even(n: Int) = n % 2 == 0

  it should "work" in {
    def testTakeWhile(as: Stream[Int], expected: List[Int]) = assertResult(expected)(as.takeWhile(even).toList)

    val tests = Table(
      ("as: Stream[Int]", "as.takeWhile(even)"),
      (Stream(), Nil),
      (Stream(0, 2, 3), List(0, 2)),
      (Stream(1, 2), Nil))
    forAll(tests)(testTakeWhile)
  }

  it should "for all l: List[Int] ==> Stream(l).takeWhile(even).toList == l.takeWhile(even)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.takeWhile(even))(Stream(l: _*).takeWhile(even).toList)
    }
  }

  behavior of "5.4 forAll"

  it should "work" in {
    def testForAll(as: Stream[Int], expected: Boolean) = assertResult(expected)(as.forAll(even))

    val tests = Table(
      ("as: Stream[Int]", "as.forAll(even)"),
      (Stream(), true),
      (Stream(0, 2, 4), true),
      (Stream(0, 1, 2), false))
    forAll(tests)(testForAll)
  }

  it should "for all l: List[Int] ==> Stream(l).forAll(even).toList == l.forall(even)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.forall(even))(Stream(l: _*).forAll(even))
    }
  }

  behavior of "5.5 takeWhileViaFoldRight"

  it should "work" in {
    def testTakeWhileViaFoldRight(as: Stream[Int], expected: List[Int]) =
      assertResult(expected)(as.takeWhileViaFoldRight(even).toList)

    val tests = Table(
      ("as: Stream[Int]", "as.takeWhile(even)"),
      (Stream(), Nil),
      (Stream(0, 2, 3), List(0, 2)),
      (Stream(1, 2), Nil))
    forAll(tests)(testTakeWhileViaFoldRight)
  }

  it should "for all l: List[Int] ==> Stream(l).takeWhile(even).toList == l.takeWhile(even)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.takeWhile(even))(Stream(l: _*).takeWhileViaFoldRight(even).toList)
    }
  }

  behavior of "5.6 headOption"

  it should "work" in {
    def testHeadOption[A](as: Stream[A], expected: Option[A]) =
      assertResult(expected)(as.headOption)

    val tests = Table(
      ("as: Stream[_]", "as.headOption"),
      (Stream(), None),
      (Stream(1, 2, 3), Some(1)),
      (Stream("a"), Some("a")))
    forAll(tests)(testHeadOption)
  }

  it should "for all l: List[_] ==> Stream(l).headOption == l.headOption" in {
    forAll("l: List[_]") { l: List[Int] =>
      assertResult(l.headOption)(Stream(l: _*).headOption)
    }
  }

  behavior of "5.7.1 map"

  def plusX[A](a: A) = a.toString + "x"

  it should "work" in {
    def testMap(as: Stream[_], expected: List[String]) =
      assertResult(expected)(as.map(plusX).toList)

    val tests = Table(
      ("as: Stream[_]", "as.map(...)"),
      (Stream(), Nil),
      (Stream(1, 2, 3), List("1x", "2x", "3x")),
      (Stream("a", "b", "c"), List("ax", "bx", "cx")))
    forAll(tests)(testMap)
  }

  it should "for all l: List[Int] ==> Stream(l).map(...).toList == l.map(...)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.map(plusX))(Stream(l: _*).map(plusX).toList)
    }
  }

  behavior of "5.7.2 filter"

  it should "work" in {
    def testFilter(as: Stream[Int], expected: List[Int]) =
      assertResult(expected)(as.filter(even).toList)

    val tests = Table(
      ("as: Stream[Int]", "as.filter(even)"),
      (Stream(), Nil),
      (Stream(0, 1, 2, 3, 4), List(0, 2, 4)),
      (Stream(1, 3, 5, 7), Nil))
    forAll(tests)(testFilter)
  }

  it should "for all l: List[Int] ==> Stream(l).filter(even).toList == l.filter(even)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.filter(even))(Stream(l: _*).filter(even).toList)
    }
  }

  behavior of "5.7.3 append"

  it should "work" in {
    def testAppend[A](as: Stream[A], as2: Stream[A], expected: List[A]) =
      assertResult(expected)(as.append(as2).toList)

    val tests = Table(
      ("as: Stream[A]", "as1: Stream[A]", "as.append(as2)"),
      (Stream(), Stream(), Nil),
      (Stream(0, 1, 2), Stream(3, 4), List(0, 1, 2, 3, 4)),
      (Stream("a", "b"), Stream("c", "d"), List("a", "b", "c", "d")))
    forAll(tests)(testAppend)
  }

  it should "for all l, l1: List[Int] ==> Stream(l).append(Stream(l1)).toList == l ++ l1" in {
    forAll("l: List[Int]", "l1: List[Int]") { (l: List[Int], l1: List[Int]) =>
      assertResult(l ++ l1)(Stream(l: _*).append(Stream(l1: _*)).toList)
    }
  }

  behavior of "5.7.4 flatMap"

  def plusXStream[A](a: A) = Stream(plusX(a))
  def plusXList[A](a: A) = List(plusX(a))

  it should "work" in {
    def testMap(as: Stream[_], expected: List[String]) =
      assertResult(expected)(as.flatMap(plusXStream).toList)

    val tests = Table(
      ("as: Stream[_]", "as.flatMap(...)"),
      (Stream(), Nil),
      (Stream(1, 2, 3), List("1x", "2x", "3x")),
      (Stream("a", "b", "c"), List("ax", "bx", "cx")))
    forAll(tests)(testMap)
  }

  it should "for all l: List[Int] ==> Stream(l).map(...).toList == l.map(...)" in {
    forAll("l: List[Int]") { l: List[Int] =>
      assertResult(l.flatMap(plusXList))(Stream(l: _*).flatMap(plusXStream).toList)
    }
  }

}