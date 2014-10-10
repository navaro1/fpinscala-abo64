package fpinscala.monads

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import Monad._

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonadSpec extends FlatSpec with PropertyChecks {

//  implicit def toMonadOps[A,M[_] <: Monad[M]](self: M[A]) = new MonadOps(self)
  private def checkMonadLaw[A,B,C,M[_] <: Monad[M]](x: M[A], f: A => M[B], g: B => M[C]) = {
//    import x.toMonadOps
//    x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
//    val fx = x.flatMap(x)(f)
//    fx.flatMap(fx)(g) == x.flatMap(x)(a => f(a).flatMap(g))
//    x.compose(x.compose(f, g), h) == compose(f, compose(g, h))
  }

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

  private[MonadSpec] class ComposeTest[F[_]](M: Monad[F]) {
    type T = Int
    def kleisli[B](f: T => B) = (a: T) => M.unit[B](f(a))
    val f = kleisli(_ + 1)
    val g = kleisli(_ + 2)
    val h = kleisli(_ + 4)
    val fg = kleisli(_ + 3)
    val fgh = kleisli(_ + 7)
    val compose = M.compose[T,T,T] _

    def testCompose =
      forAll("n") { n: T =>
        assert(compose(f, g)(n) == fg(n))
        assert(compose(compose(f, g), h)(n) == fgh(n))
      }

    def testAssociativeLaw =
      forAll("n") { n: T =>
        assert(compose(compose(f, g), h)(n) == compose(f, compose(g, h))(n))
      }
  }

  val listComposeTest = new ComposeTest(listMonad)
  it should "work in ListMonad" in listComposeTest.testCompose
  it should "obey the associative law in ListMonad" in listComposeTest.testAssociativeLaw

  val optionComposeTest = new ComposeTest(optionMonad)
  it should "work in OptionMonad" in optionComposeTest.testCompose
  it should "obey the associative law in OptionMonad" in optionComposeTest.testAssociativeLaw
}