package fpinscala.iomonad

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import IO3._

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class IOSpec extends FlatSpec with PropertyChecks {

  behavior of "13.1 freeMonad"
  it should "work" in {
    val listFreeMonad = freeMonad[List]
    import listFreeMonad._
    forAll("a") { a: Int =>
      val fm = flatMap(unit(a))(unit(_))
      val r = fm match {
        case FlatMap(Return(a), f) => f(a)
        case x => fail(s"unexpected: $x")
      }
      assert(r == Return(a))
    }
  }
}