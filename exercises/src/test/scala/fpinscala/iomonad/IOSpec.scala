package fpinscala.iomonad

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import IO3._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import java.io.ByteArrayInputStream

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class IOSpec extends FlatSpec with PropertyChecks {

  implicit def arbFree[F[_], A](implicit aa: Arbitrary[A], afa: Arbitrary[F[A]]): Arbitrary[Free[F, A]] =
  {
    val returnGen: Gen[Return[F,A]] = arbitrary[A] map(IO3.Return(_))
    val suspendGen: Gen[Suspend[F,A]] = arbitrary[F[A]] map(Suspend(_))
    def freeGen(depth: Int): Gen[Free[F,A]] =
        if (depth == 0) Gen.oneOf(returnGen, suspendGen)
        else Gen.oneOf(returnGen, suspendGen, flatMapGen(depth - 1))
    def flatMapGen(depth: Int): Gen[FlatMap[F,A,A]] = {
      for {
        s <- freeGen(depth - 1)
        f <- freeGen(depth - 1) map((a:A) => _)
      } yield FlatMap(s, f)
    }

    val MaxDepth = 2 // avoid StackOverflow
    Arbitrary(freeGen(MaxDepth))
  }

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

  behavior of "13.2 runTrampoline"
  it should "work" in {
    implicit def arbFunction0[R](implicit a: Arbitrary[R]): Arbitrary[Function0[R]] =
      Arbitrary(arbitrary[R] map(() => _))
    def eval[A](free: Free[Function0, A]): A = free match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(s: Free[Function0, A],f) => eval(f(eval(s)))
    }
    forAll("a") { a: Free[Function0,Int] =>
      assert(runTrampoline(a) == eval(a))
    }
  }

  private def evalFreeList[A](free: Free[List, A]): List[A] = free match {
    case Return(a) => List(a)
    case Suspend(r) => r
    case FlatMap(s: Free[List, A], f) => evalFreeList(s) flatMap (a => evalFreeList(f(a)))
  }

  behavior of "13.3 run"
  it should "work" in {
    implicit val listMonad =
      new Monad[List] {
        override def unit[A](a: => A) = List(a)
        override def flatMap[A,B](as: List[A])(f: A => List[B]) = as flatMap f
      }
    forAll("a") { a: Free[List,Int] =>
      assert(IO3.run(a) == evalFreeList(a))
    }
  }

  behavior of "13.4.1 translate"
  it should "work" in {
    val optionToList = new (Option ~> List) {
      override def apply[A](o: Option[A]) = o map(List(_)) getOrElse(List())
    }
    def optionToListFree[A](fo: Free[Option,A]): Free[List,A] = fo match {
      case Return(a) => IO3.Return[List,A](a)
      case Suspend(o) => Suspend[List,A](optionToList(o))
      case FlatMap(s: Free[Option,A],f) => FlatMap[List,A,A](optionToListFree(s), (a:A) => optionToListFree(f(a)))
    }
    implicit val arbOptionListFree: Arbitrary[(Free[Option,Int], Free[List,Int])] = Arbitrary(for {
      fo <- arbitrary[Free[Option,Int]]
      fl = optionToListFree(fo)
    } yield (fo,fl))
    forAll("(fo,fl)") { pf: (Free[Option,Int], Free[List,Int]) =>
      val (fo, fl) = pf
      val translation = translate(fo)(optionToList)
      assert(evalFreeList(translation) == evalFreeList(fl))
    }
  }

  behavior of "13.4.2 runConsole"
  it should "work" in {
    type ConsoleResult = Option[String]
    def toStringConsoleResult[String](any: Any): ConsoleResult = any match {
      case () => None
      case None => None
      case Some(s) => Some(s.toString)
      case s => Some(s.toString)
    }
    val inputLine = "Monty Python"
    implicit val arbStr: Arbitrary[String] = Arbitrary(
        Gen.oneOf("always", "look", "at", "the", "bright", "side", "of", "life"))
    val readLineGen = Gen.const(ReadLine)
    val printLineGen = arbitrary[String] map(PrintLine(_))
    def genConsole[A] =
      // is there a better way w/o asInstanceOf?
      Gen.oneOf[Console[_]](readLineGen, printLineGen) map(_.asInstanceOf[Console[A]])
    implicit def arbConsole[A]: Arbitrary[Console[A]] = Arbitrary(genConsole)
//    implicit def arbFreeAnyRef: Free[Console,ConsoleResult] =
//      Arbitrary {
//    val optionToConsole = new (Option ~> Console) {
//      override def apply[A](o: Option[_]) = o map(a => PrintLine(a.toString)) getOrElse(ReadLine)
//    }
//      def toConsoleResultFree(fo: Free[Option,String]): Free[Console,ConsoleResult] = fo match {
//          case Return(a) => IO3.Return[Console, ConsoleResult](Some(a))
//          case Suspend(o) => Suspend[Console, ConsoleResult](optionToConsole(o))
//          case FlatMap(s: Free[Option, String], f) => FlatMap[Console, ConsoleResult](optionToConsole(s), (a: A) => optionToListFree(f(a)))
//        }
//      arbitrary[Free[Option,Boolean]] map(translate(_)(toConsoleResultFree))
//    }
    def eval[A](free: Free[Console, A]): ConsoleResult = free match {
      case IO3.Return(a) => toStringConsoleResult(a)
      case Suspend(ReadLine) => toStringConsoleResult(inputLine)
      case Suspend(r) => toStringConsoleResult(r.toThunk())
      case FlatMap(s: Free[Console, A],f) => eval(f(eval(s)))
    }
    val in = new ByteArrayInputStream((s"$inputLine\n" * 100).getBytes)
    scala.Console.withIn(in) {
//        println(s"evalResultX=${eval(IO3.FlatMap(IO3.Return(Some("side")), (_:Any) => Return("of")))}")
      forAll("a") { a: Free[Console,ConsoleResult] =>
        println(s"a=$a")
        val result = runConsole(a)
        println(s"result=$result")
        val evalResult = eval(a)
        println(s"evalResult=$evalResult")
        assert(toStringConsoleResult(result) == evalResult)
      }
    }
  }
}
