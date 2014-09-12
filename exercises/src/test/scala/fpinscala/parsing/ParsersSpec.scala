package fpinscala.parsing

import scala.util.matching.Regex

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParsersSpec extends FlatSpec with PropertyChecks {

  private type ParseSuccess[+A] = (A,Location)
  private type ParseResult[+A] = Either[ParseError,ParseSuccess[A]]
  private type Parser[+A] = Location => ParseResult[A]

  private object TestParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
      p(Location(input, 0)) match {
        case Right((a, _)) => Right(a)
        case Left(e) => Left(e)
      }
    implicit override def string(s: String): Parser[String] = {
      in => if (input(in).startsWith(s)) Right((s, in.advanceBy(s.length)))
            else parseError(in, s"""string: "${input(in)}" != "$s"""")
    }
    override def succeed[A](a: A): Parser[A] =
      in => Right((a, in))
    implicit override def regex(r: Regex): Parser[String] = {
      in => r.findPrefixOf(input(in)) match {
          case Some(prefix) => Right((prefix, in.advanceBy(prefix.length)))
          case _ => parseError(in, s"""regex: "${input(in)}" does not start with regex "$r"""")
        }
    }
    override def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = {
      in => f(in) match {
        case Right((a,in1)) => g(a)(in1)
        case left@Left(_) => left.asInstanceOf[ParseResult[B]]
      }
    }
    override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = {
      in => p1(in) match {
        case left@Left(_) => p2(in)
        case success => success
      }
    }
    override def slice[A](p: Parser[A]): Parser[String] = {
      def slice(loc: Location, n: Int) = loc.input.substring(loc.offset, loc.offset + n)
      in => p(in) match {
        case Right((a,in1)) => Right(slice(in, in1.offset), in1)
        case left@Left(_) => left.asInstanceOf[ParseResult[String]]
      }
    }

    private def input(loc: Location): String = loc.input.substring(loc.offset)
    def parseError[A](loc: Location, msg: String): ParseResult[A] = Left(ParseError(List((loc, msg))))
  }

  import TestParser._

  private def limitedStringGen(min: Int, max: Int) =
    Gen.choose(min, max) flatMap { l => Gen.listOfN(l, Gen.alphaNumChar)} map(_.mkString)

  implicit val arbStringParser: Arbitrary[Parser[String]] =
//    Arbitrary(arbitrary[String] map string)
    Arbitrary(limitedStringGen(1, 10) map string)

  behavior of "9.1.1 map2ViaProduct"

  it should "work" in {
    import Exercises.map2ViaProduct
    val parser: Parser[String] = map2ViaProduct("abra", "cadabra")(_ + _)
    assert(TestParser.run(parser)("abracadabra") == Right("abracadabra"),
        """run(parser)("abracadabra")""")
    assert(TestParser.run(parser)("abracadabrax") == Right("abracadabra"),
        """run(parser)("abracadabrax")""")

    assert(TestParser.run(parser)("abra cadabra") ==
      parseError(Location("abra cadabra", 4), s"""string: " cadabra" != "cadabra""""),
        """run(parser)("abra cadabra")""")
  }

  behavior of "9.1.2 many1"

  it should "work" in {
    val parser: Parser[List[Char]] = many1(char('a'))
    assert(TestParser.run(parser)("aaa") == Right(List.fill(3)('a')), """run(parser)("aaa")""")

    assert(TestParser.run(parser)("baaa") ==
      parseError(Location("baaa", 0), s"""string: "baaa" != "a""""),
        """run(parser)("baaa")""")
  }

  behavior of "9.2 product law"

  it should "hold" in {
    forAll(limitedStringGen(1, 10) label "p") { s: String =>
      val p: Parser[String] = string(s)
      equal(product(p,p), p.map((a:String) => (a,a)))
    }
  }

  behavior of "9.3 many"

  it should "work" in {
    val parser: Parser[List[Char]] = many(char('a'))
    assert(TestParser.run(parser)("aaa") == Right(List.fill(3)('a')), """run(parser)("aaa")""")
    assert(TestParser.run(parser)("baaa") == Right(List()), """run(parser)("baaa")""")
    assert(TestParser.run(parser)("") == Right(List()), """run(parser)("")""")
  }

  behavior of "9.4 listOfN"

  it should "work" in {
    // seems to be a bug in the book: return type of listOfN is Parser[List[A]], not Parser[A]
    assert(TestParser.run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab", "ab", "cad")),
        """150: run(listOfN(3, "ab" | "cad"))("ababcad")""")
    assert(TestParser.run(listOfN(3, "ab" | "cad"))("cadabab") == Right(List("cad", "ab", "ab")),
        """150: run(listOfN(3, "ab" | "cad"))("cadabab")""")
    assert(TestParser.run(listOfN(3, "ab" | "cad"))("ababab") == Right(List("ab", "ab", "ab")),
        """150: run(listOfN(3, "ab" | "cad"))("ababab")""")
    assert(TestParser.run(listOfN(3, "ab" | "cad"))("abababx") == Right(List("ab", "ab", "ab")),
        """run(listOfN(3, "ab" | "cad"))("abababx")""")

    assert(TestParser.run(listOfN(3, "ab" | "cad"))("ababaxb") ==
      parseError(Location("ababaxb", 4), s"""string: "axb" != "cad""""),
        """run(listOfN(3, "ab" | "cad"))("ababaxb")""")
  }

  behavior of "9.6 csListOfN"

  import TestParser.Exercises.csListOfN

  it should "succeed for \"4aaaa\"" in {
    val parser: Parser[List[Char]] = csListOfN(char('a'))
    assert(TestParser.run(parser)("4aaaa") == Right(List.fill(4)('a')))
  }

  it should "fail for \"4aaa\"" in {
    assert(TestParser.run(csListOfN(char('a')))("4aaa") ==
           parseError(Location("4aaa", 4), s"""string: "" != "a""""))
  }

  it should "obey the law on page 157"  in {
    val parser: Parser[Char] = char('a')
    val strSizeGen = Gen.chooseNum(0, 10) label "n"
    forAll(strSizeGen) { n: Int =>
      assert(TestParser.run(csListOfN(parser))(n + ("a" * n)).right.get.size == n)
    }
  }

  behavior of "9.7.1 product"

  it should "work" in {
    val as = many1(char('a')) map((_:List[Char]).mkString)
    val bs = many1(char('b')) map((_:List[Char]).mkString)
    val abProduct = product(as, bs)
    assert(TestParser.run(abProduct)("ab") == Right(("a","b")), """run(abProduct)("ab")""")
    assert(TestParser.run(abProduct)("aabb") == Right(("aa","bb")), """run(abProduct)("aabb")""")

    assert(TestParser.run(abProduct)("") ==
      parseError(Location("", 0), s"""string: "" != "a""""), """run(abProduct)("")""")
    assert(TestParser.run(abProduct)("xab") ==
      parseError(Location("xab", 0), s"""string: "xab" != "a""""), """run(abProduct)("xab")""")
    assert(TestParser.run(abProduct)("axb") ==
      parseError(Location("axb", 1), s"""string: "xb" != "b""""), """run(abProduct)("axb")""")
  }

  behavior of "9.7.2 map2"

  it should "work" in {
    val parser: Parser[String] = map2("abra", "cadabra")(_ + _)
    assert(TestParser.run(parser)("abracadabra") == Right("abracadabra"),
        """run(parser)("abracadabra")""")
    assert(TestParser.run(parser)("abracadabrax") == Right("abracadabra"),
        """run(parser)("abracadabrax")""")

    assert(TestParser.run(parser)("abra cadabra") ==
      parseError(Location("abra cadabra", 4), s"""string: " cadabra" != "cadabra""""),
        """run(parser)("abra cadabra")""")
  }

  behavior of "9.8 map"

  private def equal[A](p1: Parser[A], p2: Parser[A]) = {
    forAll("in") { in: String => assert(TestParser.run(p1)(in) == TestParser.run(p2)(in)) }
  }

  it should "preserve structure, p.150" in {
    forAll("p") { p: Parser[String] => equal(p, p.map(identity[String])) }
//    forAll("p", "in") {(p: Parser[String], in: String) =>
//      assert(TestParser.run(p)(in) == TestParser.run(p.map(identity[String]))(in))
//    }
  }

  behavior of "Laws"

  it should "hold for 149: singleCharLaw" in {
    import Laws._
    forAll("c") { c: Char => assert(singleCharLaw(c), s"singleCharLaw($c)") }
  }

  it should "hold for 149: singleStringLaw" in {
    import Laws._
    forAll("s") { s: String => assert(singleStringLaw(s), s"""singleStringLaw("$s")""") }
  }

  it should "hold for 153: succeedLaw" in {
    import Laws._
    forAll("n", "s") { (n: Int, s: String) =>
      assert(succeedLaw(n)(s), s"""succeedLaw("$n", "$s")""") }
  }

  it should "hold for 154: numALaw" in {
    forAll("p") {(p: Parser[String]) =>
      equal(p.many.map((_: List[String]).size), slice(p.many).map((_: String).size))
    }
  }

  behavior of "Facts"

  it should "all be true" in {
    import TestParser.Facts._
    facts.foreach {case (k, v) => assert(v, k)}
  }
}
