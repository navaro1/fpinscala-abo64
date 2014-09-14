package fpinscala.parsing

import scala.util.matching.Regex

object ParserTypes { // 167
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing] // 169
}

object ParserImpl extends Parsers[ParserTypes.Parser] {

  import ParserTypes._

  override def run[A](p: Parser[A])(input: String): Either[ParseError,A] = // 149, 163
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

  private def input(loc: Location): String = loc.input.substring(loc.offset)

  override implicit def string(s: String): Parser[String] = { // 149
    def headMatches(s1: String, s2: String): Boolean =
      s1.headOption.flatMap(c1 => s2.headOption.map(c2 => c1 == c2)).getOrElse(true)

    loc =>
      if (input(loc).startsWith(s)) Success(s, s.length)
      else Failure(loc.toError(s"""string: "${input(loc)}" does not start with "$s""""),
          headMatches(input(loc), s))
  }

  override implicit def regex(r: Regex): Parser[String] = // 157
    loc =>
      r.findPrefixOf(input(loc)) match {
        case Some(prefix) => Success(prefix, prefix.length)
        case _ => Failure(loc.toError(s"""regex: "${input(loc)}" does not start with regex "$r""""), false)
      }

  override def slice[A](p: Parser[A]): Parser[String] = { // 154
    def slice(loc: Location, n: Int) = loc.input.substring(loc.offset, loc.offset + n)

    loc => p(loc) match {
      case Success(_, n) => Success(slice(loc, n), n)
      case f@Failure(_,_) => f
    }
  }
  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ??? // 161
  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ??? // 162
  override def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ??? // 157
  override def attempt[A](p: Parser[A]): Parser[A] = ??? // 164
  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ??? // 149, 156
  override def succeed[A](a: A): Parser[A] = // 153
    _ => Success(a, 0)

}