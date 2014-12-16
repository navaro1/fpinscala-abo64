package fpinscala.parallelism

import java.util.concurrent._
import scala.collection.mutable.ArraySeq

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    (es: ExecutorService) =>
      {
        val cf = c(es)
        map2(a, b)((a, b) => f(a, b, cf.get))(es)
      }
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => Par.lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def mergeSortPar[T](parSeq: Par[Seq[T]])(implicit ord: Ordering[T]): Par[Seq[T]] = {
    //    def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] = {
    //      val len1 = seq1.length 
    //      val len2 = seq2.length
    //      val len = len1+ len2
    //      val arr = new ArraySeq[T](len)
    //      var (i1, i2, ia) = (0, 0, 0)
    //      while (ia < len) {
    //        if (i2 == len2 || (i1 < len1 && ord.lteq(seq1(i1), seq2(i2)))) {
    //          arr(ia) = seq1(i1)
    //          i1 += 1
    //        } else {
    //          arr(ia) = seq2(i2)
    //          i2 += 1
    //        }
    //        ia += 1
    //      }
    //      arr.toSeq
    //    }

    // less efficient, but more readable - and no side effects! ;-)
    // here is a discussion: http://dublintech.blogspot.de/2013/05/how-could-scala-do-merge-sort.html
    def merge(seq1: Seq[T], seq2: Seq[T]): Seq[T] =
      (seq1, seq2) match {
        case (Nil, _) => seq2
        case (_, Nil) => seq1
        case (h1 :: t1, h2 :: t2) =>
          if (ord.lt(h1, h2)) h1 +: merge(t1, seq2) // :: as constructor only defined for List
          else h2 +: merge(seq1, t2)
      }

    parSeq flatMap { seq =>
      val len = seq.length
      if (len <= 1) Par.unit(seq)
      else {
        val (left, right) = seq.splitAt(len / 2)
        Par.map2(fork(mergeSortPar(Par.unit(left))), fork(mergeSortPar(Par.unit(right))))(merge)
      }
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      {
        val i = n(es).get
        run(es)(choices(i))
      }
  }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(if (_) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es =>
      {
        val k = key(es).get
        run(es)(choices(k))
      }
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = pa(es).get
      run(es)(choices(a))
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val a = pa(es).get
      run(es)(f(a))
    }

  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => {
      val pa = ppa(es).get
      pa(es)
    }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(x => x)

  /* Gives us infix syntax for `Par`. */
  //  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // since Scala 2.10: implicit class instead of implicit converter function
  implicit class ParOps[A](p: Par[A]) {
    def run(s: ExecutorService): Future[A] = Par.run(s)(p)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  // Kestrel combinator: perform some side effect before returning result
  // http://stackoverflow.com/questions/9671620/how-to-keep-return-value-when-logging-in-scala/9673294#9673294
  // http://debasishg.blogspot.de/2009/09/side-effects-with-kestrel-in-scala.html
  private def kestrel[A](x: A)(f: A => Unit): A = { f(x); x }

  def parCountWords(paragraphs: List[String]): Par[Int] = {
    val wordPattern = """(?U)\b\w+\b""".r
    def countWords(s: String): Int =
      kestrel(wordPattern.findAllIn(s).size) { wc =>
//        println(s"${Thread.currentThread} $s: $wc")
      }
    def countWordsPar(s: String): Par[Int] = Par.lazyUnit(countWords(s))

    val parCounts: List[Par[Int]] = paragraphs.map(countWordsPar)
    Par.sequence(parCounts) map (_.sum)
  }
}

