package fpinscala.parsing

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParserImplSpec extends FlatSpec with PropertyChecks with ParserTest[ParserTypes.Parser] {

  override val P = ParserImpl
  import ParserTypes._
  import ParserImpl._

  behavior of "9.13.1 string"

  it should "work" in {
    forAll(limitedStringGen(1, 10) label "s") { s: String =>
      val p: Parser[String] = string(s)
      assert(p(Location(s)) == Success(s, s.length))
      val loc = Location("_" + s)
      assert(p(loc) == Failure(loc.toError(s"Expected: $s"), false))
    }
  }
}