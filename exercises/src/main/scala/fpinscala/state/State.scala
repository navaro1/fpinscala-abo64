package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rnd, state) = rng.nextInt
    val nonNegative = if (rnd == Int.MinValue) 0 else math.abs(rnd)
    (nonNegative, state)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (positiveInt, rng1) = nonNegativeInt(rng)
    (positiveInt.toDouble / (Int.MaxValue.toDouble + 1.0d), rng1)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng1) = rng.nextInt
    val (db, rng2) = double(rng1)
    ((int, db), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, db), rng1) = intDouble(rng)
    ((db, int), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (db1, rng1) = double(rng)
    val (db2, rng2) = double(rng1)
    val (db3, rng3) = double(rng2)
    ((db1, db2, db3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) (Nil, rng)
    else {
      val (int, rng1) = rng.nextInt
      val (intz, rng2) = ints(count - 1)(rng1)
      (int :: intz, rng2)
    }
  }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs match {
      case Nil => (Nil, rng)
      case h :: t =>
        val (a, rnga) = h(rng)
        map(sequence(t))(acc => a :: acc)(rnga)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rnga) = f(rng)
    g(a)(rnga)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(
      (state: S) => {
        val (a, next) = run(state)
        (f(a), next)
      }
    )
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      (state: S) => {
        val (a, next1) = run(state)
        val (b, next2) = sb.run(next1)
        (f(a, b), next2)
      }
    )


  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      (state: S) => {
        val (a, next) = run(state)
        f(a).run(next)
      }
    )
}



object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = sas match {
    case Nil => unit(Nil)
    case h :: t => State(
      state => {
        val (a, next) = h.run(state)
        sequence(t).map(acc => a :: acc).run(next)
      }
    )


  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case Nil => State(machine => ((machine.coins, machine.candies), machine))
    case input :: as => State(machine => {
      val next = serveInput(input)(machine)
      simulateMachine(as).run(next)
    })
  }

  private def serveInput(input: Input)(machine: Machine): Machine = {
    val Machine(locked, candies, coins) = machine
    input match {
      case _ if candies < 1 => machine
      case Coin if locked => Machine(locked = false, candies, coins + 1)
      case Turn if !locked => Machine(locked = true, candies - 1, coins )
      case _ => machine
    }
  }
}