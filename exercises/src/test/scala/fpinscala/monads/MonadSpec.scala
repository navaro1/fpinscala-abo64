package fpinscala.monads

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Monad._
import java.util.concurrent.Executors

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonadSpec extends FlatSpec with PropertyChecks {

  private[MonadSpec] class MonadTest[F[_]](M: Monad[F]) {
    import M._
    type T = Int
    def kleisli[B](f: T => B) = (a: T) => unit[B](f(a))
    val f = kleisli[T](_ + 1)
    val g = kleisli(_ + 2)
    val h = kleisli(_ + 4)
    val fg = kleisli(_ + 3)
    val fgh = kleisli(_ + 7)

    def mapPreservesStructure(eq: (F[T], F[T]) => Boolean = (_ == _)) =
      forAll("n") { n: T =>
        val m = unit[T](n)
        val idM = map(m)(identity[T])
        assert(eq(idM, m), s"""eq($idM, $m)""")
      }

    def testCompose =
      forAll("n") { n: T =>
        assert(compose(f, g)(n) == fg(n))
        assert(compose(compose(f, g), h)(n) == fgh(n))
      }

    def associativeLaw =
      forAll("n") { n: T =>
        assert(compose(compose(f, g), h)(n) == compose(f, compose(g, h))(n))
      }

    def identityLaws = {
      val strictUnit = (t: T) => unit(t)
      val leftIdentity = compose(f, strictUnit)
      val rightIdentity = compose(unit[T], f)

      forAll("n") { n: T =>
        assert(leftIdentity(n) == f(n), "leftIdentity")
        assert(rightIdentity(n) == f(n), "rightIdentity")
        assert(flatMap(f(n))(strictUnit) == f(n), "leftIdentity (flatMap)")
        assert(flatMap(strictUnit(n))(f) == f(n), "rightIdentity (flatMap)")
      }
    }
  }

  val listMonadTest = new MonadTest(listMonad)
  val optionMonadTest = new MonadTest(optionMonad)
  val parMonadTest = new MonadTest(parMonad)
//  val parserMonadTest = new MonadTest(parserMonad())

  behavior of "11.1.1 parMonad"
  it should "work" in {
    import fpinscala.parallelism.Par.{run => prun}
    val es = Executors.newCachedThreadPool
    try {
      parMonadTest.mapPreservesStructure((p1,p2) => prun(es)(p1) == prun(es)(p2))
    } finally {
      es.shutdown
    }
  }

  behavior of "11.1.2 ParserMonad"

  behavior of "11.1.3 optionMonad"
  it should "work" in optionMonadTest.mapPreservesStructure()

  behavior of "11.1.4 streamMonad"

  behavior of "11.1.5 listMonad"
  it should "work" in listMonadTest.mapPreservesStructure()

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

  behavior of "11.6.1 filterM in ListMonad"
  it should "work" in {
    def evenList(i: Int): List[Boolean] = List(i % 2 == 0)
    val tests =
      Table(
        ("la: List[Int]", "filterM(ma)(evenList"),
        (List[Int](), List(List[Int]())),
        (List(1,3), List(List[Int]())),
        (List(2,4), List(List(2,4))),
        (List(1,2,3,4), List(List(2,4))))
    forAll(tests) { (la: List[Int], expected: List[List[Int]]) =>
      assert(listMonad.filterM(la)(evenList) == expected)
    }
  }

  behavior of "11.6.2 filterM in OptionMonad"
  it should "work" in {
    def evenOption(i: Int): Option[Boolean] = Some(i % 2 == 0)
    val tests =
      Table(
        ("la: List[Int]", "filterM(ma)(evenOption"),
        (List[Int](), Some(List[Int]())),
        (List(1,3), Some(List[Int]())),
        (List(2,4), Some(List(2,4))),
        (List(1,2,3,4), Some(List(2,4))))
    forAll(tests) { (la: List[Int], expected: Some[List[Int]]) =>
      assert(optionMonad.filterM(la)(evenOption) == expected)
    }
  }

  behavior of "11.7 compose"

  it should "work in ListMonad" in listMonadTest.testCompose
  it should "obey the associative law in ListMonad" in listMonadTest.associativeLaw

  it should "work in OptionMonad" in optionMonadTest.testCompose
  it should "obey the associative law in OptionMonad" in optionMonadTest.associativeLaw

  behavior of "11.8 flatMapViaCompose"

  behavior of "identity laws"
  it should "work in ListMonad" in listMonadTest.identityLaws
  it should "work in OptionMonad" in optionMonadTest.identityLaws
}