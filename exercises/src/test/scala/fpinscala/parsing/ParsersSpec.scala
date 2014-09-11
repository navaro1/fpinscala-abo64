package fpinscala.parsing

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import scala.util.matching.Regex

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ParsersSpec extends FlatSpec with PropertyChecks with Matchers  {

  private type ParseSuccess[+A] = (A,Location)
  private type ParseResult[+A] = Either[ParseError,ParseSuccess[A]]
  private type Parser[+A] = Location => ParseResult[A]

  private object TestParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
      p(Location(input, 0)) match {
        case Right((a, _)) => Right(a)
        case Left(e) => Left(e)
      }
    override def string(s: String): Parser[String] = {
      in => if (input(in).startsWith(s)) Right((s, in.advanceBy(s.length)))
            else parseError(in, s"""string: "${input(in)}" != "$s"""")
    }
    override def succeed[A](a: A): Parser[A] =
      in => Right((a, in))
    override def regex(r: Regex): Parser[String] = {
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
      in => p(in) match {
        case Right((a,in1)) => Right((input(in), in1))
        case left@Left(_) => left.asInstanceOf[ParseResult[String]]
      }
    }

    private def input(loc: Location): String = loc.input.substring(loc.offset)
    def parseError[A](loc: Location, msg: String): ParseResult[A] = Left(ParseError(List((loc, msg))))
  }

  import TestParser._

  behavior of "9.1.1 map2"

  it should "work" in {
    val parser: Parser[String] = map2(TestParser.regex("abra".r), string("cadabra"))(_ + _)
    assert(TestParser.run(parser)("abracadabra") == Right("abracadabra"))

    val errorLoc = Location("abra cadabra", 4)
    assert(TestParser.run(parser)("abra cadabra") ==
      TestParser.parseError(errorLoc, s"""string: " cadabra" != "cadabra""""))
  }

  behavior of "9.1.2 many1"

  it should "work" in {
    val parser: Parser[List[Char]] = many1(char('a'))
    assert(TestParser.run(parser)("aaa") == Right(List.fill(3)('a')))
  }

  behavior of "9.3 many"

  behavior of "9.4 listOfN"

  behavior of "9.6 csListOfN"

  it should "succeed for \"4aaaa\"" in {
    val parser: Parser[List[Char]] = TestParser.Exercises.csListOfN(char('a'))
    assert(TestParser.run(parser)("4aaaa") == Right(List.fill(4)('a')))
  }

  it should "fail for \"4aaa\"" in {
    val errorLoc = Location("4aaa", 4)
    assert(TestParser.run(TestParser.Exercises.csListOfN(char('a')))("4aaa") ==
           TestParser.parseError(errorLoc, s"""string: "" != "a""""))
  }

  behavior of "9.7.1 product"

  behavior of "9.7.2 map2"

  behavior of "9.8 map"

  behavior of "Facts"

  it should "all be true" in {
    TestParser.Facts.facts.foreach {case (k, v) => assert(v, k)}
  }
}
