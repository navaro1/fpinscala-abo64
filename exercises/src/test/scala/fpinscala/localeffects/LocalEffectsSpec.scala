package fpinscala.localeffects

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LocalEffectsSpec extends FlatSpec with PropertyChecks {

  behavior of "14.1 STArray.fill"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intMap = ints.zipWithIndex.map(_.swap).toMap
      val runnableST  = new RunnableST[List[Int]] {
        override def apply[S] = for {
          array <- STArray[S, Int](ints.size, 0)
          _ <- array.fill(intMap)
          l <- array.freeze
        } yield l
      }
      val result = ST.runST(runnableST)
      assert(result == ints)
    }
  }

  behavior of "14.2.1 STArray.partition"
  it should "work" in {
    val ints = Gen.choose(-10, 10)
    val intList = Gen.choose(1, 10).flatMap(Gen.listOfN(_, ints)).map(_.toSet).map(_.toList)
    val pivot = 0
    forAll(intList label "ints") { ints: List[Int] =>
      whenever(!ints.isEmpty) {
        val l = 0
        val r = ints.size - 1
        val pivot = l + (r - l) / 2
        val pivotValue = ints(pivot)
        val partitions = ints.partition(_ <= pivotValue)
        val runnableST = new RunnableST[(List[Int], Int)] {
          override def apply[S] = for {
            array <- STArray.fromList[S, Int](ints)
            p <- Immutable.partition(array, l, r, pivot)
            list <- array.freeze
          } yield (list, p)
        }
        val result = ST.runST(runnableST)
        val resultPartition = result._1.splitAt(result._2 + 1)._1
        assert(resultPartition.toSet == partitions._1.toSet)
      }
    }
  }
}
