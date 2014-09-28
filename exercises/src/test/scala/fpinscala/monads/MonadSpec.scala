package fpinscala.monads

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Monad._

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonadSpec extends FlatSpec with PropertyChecks {

  behavior of "11.1.1 parMonad"
  behavior of "11.1.2 ParserMonad"
  behavior of "11.1.3 optionMonad"
  behavior of "11.1.4 streamMonad"
  behavior of "11.1.5 listMonad"

  behavior of "11.3.1 sequence"
  behavior of "11.3.2 traverse"

  behavior of "11.4 replicateM"

  behavior of "11.5.1 replicateM in ListMonad"
  it should "work" in {
    val tests =
      Table(
        ("n", "ma: List[Int]", "replicateM(n, ma)"),
        (0, List[Int](), List(List[Int]())),
        (1, List[Int](), List()),
        (2, List[Int](), List()),
        (3, List[Int](), List()),

        (0, List(1), List(List[Int]())),
        (1, List(1), List(List(1))),
        (2, List(1), List(List(1,1))),
        (3, List(1), List(List(1,1,1))),

        (0, List(1,2), List(List[Int]())),
        (1, List(1,2), List(List(1), List(2))),
        (2, List(1,2), List(List(1,1), List(1,2), List(2,1), List(2,2))),
        (3, List(1,2), List(List(1, 1, 1), List(1, 1, 2), List(1, 2, 1), List(1, 2, 2), List(2, 1, 1),
            List(2, 1, 2), List(2, 2, 1), List(2, 2, 2)))
      )
    forAll(tests) { (n: Int, ma: List[Int], expected: List[List[Int]]) =>
      assert(listMonad.replicateM(n, ma) == expected)
    }
  }

  behavior of "11.5.2 replicateM in OptionMonad"
  it should "work" in {
    val tests =
      Table[Int, Option[Int], Option[List[Int]]](
        ("n", "ma: Option[Int]", "replicateM(n, ma)"),
        (0, None, Some(List[Int]())),
        (1, None, None),
        (2, None, None),
        (3, None, None),

        (0, Some(1), Some(List())),
        (1, Some(1), Some(List(1))),
        (2, Some(1), Some(List(1,1))),
        (3, Some(1), Some(List(1,1,1)))
      )
    forAll(tests) { (n: Int, ma: Option[Int], expected: Option[List[Int]]) =>
      assert(optionMonad.replicateM(n, ma) == expected)
    }
  }

}