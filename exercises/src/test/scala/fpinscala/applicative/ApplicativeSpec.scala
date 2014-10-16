package fpinscala.applicative

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import org.scalacheck.Gen


@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ApplicativeSpec extends FlatSpec with PropertyChecks {

  // tests w/ Int are simplest
  private type T = Int

  private implicit def arbitraryApplicative[M[_] <: Applicative[M]](m: M[T]): Arbitrary[M[T]] =
    Arbitrary(Gen.choose(-100, 100) map(m.unit(_)))

  private[ApplicativeSpec] case class ApplicativeTest[F[_]](M: Applicative[F],
      mEq: (F[Int], F[Int]) => Boolean = ((_:F[Int]) == (_:F[Int])))
{
    import M._
    def kleisli[B](f: T => B) = (a: T) => unit[B](f(a))
    val f = kleisli[T](_ + 1)
    val g = kleisli(_ + 2)
    val h = kleisli(_ + 4)
    val fg = kleisli(_ + 3)
    val fgh = kleisli(_ + 7)

    private def assertEq(m1: F[T], m2: F[T]) =
      assert(mEq(m1, m2), s"""eq($m1, $m2)""")

    def testSequence =
      forAll("l") { l: List[T] =>
        val lma = l map(M.unit(_))
        assert(sequence(lma) == unit(l))
      }

    def testTraverse =
      forAll("l") { l: List[T] =>
        assert(traverse(l)(f) == unit(l map(_ + 1)))
      }

    def testReplicateM =
      forAll(Gen.choose(0, 100) label "n") { n: Int =>
        assert(replicateM(n, unit(1)) == unit(List.fill(n)(1)))
      }
  }

  import Applicative._
  val listApplicativeTest = ApplicativeTest(listApplicative)
  val optionApplicativeTest = ApplicativeTest(optionApplicative)

  behavior of "12.1.1 sequence"
  it should "work in ListApplicative" in listApplicativeTest.testSequence
  it should "work in OptionApplicative" in optionApplicativeTest.testSequence

  behavior of "12.1.2 replicateM"
  it should "work in ListApplicative" in listApplicativeTest.testReplicateM
  it should "work in OptionApplicative" in optionApplicativeTest.testReplicateM

}