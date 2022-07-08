import scala.annotation.tailrec

object Chapter6 extends App {

  // Incorporating state into program definitions
  // Reproducible, testable.

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    // Next int will always return the same data on repeated calls on the same generator
    //
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /* Ex. 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (trial, newState) = rng.nextInt
    val betterInt = {
      if (trial <= -Int.MaxValue) Int.MaxValue
      else if (trial < 0) -trial
      else trial
    }
    (betterInt, newState)
  }

  /* Ex. 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (trial, newState) = nonNegativeInt(rng)
    (trial/Int.MaxValue.toDouble + 1,newState)
  }

  /* Ex. 6.3 */
  def intDouble(rng: RNG): ((Int,Double), RNG)= {
    val (int, newRNG) = nonNegativeInt(rng)
    val (doub, finalRNG) = double(newRNG)
    ((int, doub), finalRNG)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    intDouble(rng) match {
      case (a, newRng) => (a.swap, newRng)
    }
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doub1, rng1) = double(rng)
    val (doub2, rng2) = double(rng1)
    val (doub3, rng3) = double(rng2)
    ((doub1, doub2, doub3), rng3)
  }

  /* Ex 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def inner(n: Int, xs: List[Int])(innerRNG: RNG): (List[Int], RNG) = {
      if (n==0) (xs, innerRNG) else {
        val (newInt, newRNG) = innerRNG.nextInt
        inner(n-1, xs++List(newInt))(newRNG)
      }
    }
    inner(count, Nil)(rng)
  }


  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = (a, _)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  /* Ex. 6.5 */
  def doubleNew(rng: Rand[Int]): Rand[Double] = {
    map(rng)(_/Int.MaxValue.toDouble+1)
  }

  /* Ex. 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        // This implementation makes the second Rand dependent on the first
        // Other implementation of using same rng for both would involve a choice for final state
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  /* Ex. 6.7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // This only uses the initial rng
    rng => {
      fs.foldRight((Nil: List[A], rng)){
        case (a, (xs, eng)) => (a(eng)._1 :: xs, eng)
      }
    }
  }

  /* Ex. 6.8 */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, newRng) = f(rng)
      g(a)(newRng)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap[Int, Int](nonNegativeInt){i =>
      val mod: Int = i % n
      if (i + (n-1) - mod >= 0) rng => (mod, rng) else nonNegativeLessThan(n)
    }
  }

  /* Ex. 6.9 */
  def mapFM[A, B](ra: Rand[A])(f: A => B): Rand[B] = {
    flatMap(ra)(a => rng => (f(a), rng))
  }
  def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(i => ra(i) match {
      case (a, rng) => rb(rng) match {
        case (b, nrng) => ((a, b), nrng)
      }
    }){case (a, b) => rng => (f(a, b), rng)}
  }

  /* Ex. 6.10 */
  case class State[S,+A](run: S => (A, S)) {
    def unit[B >:A](a: B): State[S, B] = State(s => (a, s))
    def map[B](f: A => B): State[S, B] = State(
      s => {
        val (a, s1) = run(s)
        (f(a), s1)
      }
    )
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
      s => {
        val (a, s1) = run(s)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      }
    )
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      s => {
        val (a, s1) = run(s)
        val (b, s2) = f(a).run(s1)
        (b, s2)
      }
    )


  }
  object State {
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      State(s => (fs.map(_.run(s)._1), s))
    }
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
    def get[S]: State[S, S] = State(s => (s,s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }

  /* Ex 6.11 */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def changeMachine(input: Input)(machine: Machine): Machine = {
      machine match {
        case Machine(_, 0, _) => machine
        case Machine(locked, candies, coins) => input match {
          case Coin =>
            if (locked) Machine(!locked, candies, coins + 1)
            else Machine(locked, candies, coins + 1)
          case Turn =>
            if (locked) machine
            else Machine(!locked, candies-1, coins)
        }
      }
    }

    def convert(machine: Machine): ((Int, Int), Machine) = {
      ((machine.coins, machine.candies), machine )
    }

    def combined(input: Input)(machine: Machine): ((Int, Int), Machine) = convert(changeMachine(input)(machine))

    State(m => inputs.foldLeft(((m.coins, m.candies), m)){case (((_,_), m1),i) => combined(i)(m1)})
  }

//  Model solution, uses the modify and imperative for loop.
// Much cleaner/ concise than mine.

//  def update = (i: Input) => (s: Machine) =>
//    (i, s) match {
//      case (_, Machine(_, 0, _)) => s
//      case (Coin, Machine(false, _, _)) => s
//      case (Turn, Machine(true, _, _)) => s
//      case (Coin, Machine(true, candy, coin)) =>
//        Machine(false, candy, coin + 1)
//      case (Turn, Machine(false, candy, coin)) =>
//        Machine(true, candy - 1, coin)
//    }
//
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//    _ <- State.sequence(inputs map (i => State.modify(update(i))))
//    s <- State.get
//  } yield (s.coins, s.candies)

  assert(sequence(List(unit(1), unit(2), unit(3)))(SimpleRNG(1))._1  == List(1, 2, 3))
  assert(nonNegativeLessThan(10)(SimpleRNG(1))._1 <= 10)
  assert(simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(locked = true, 5, 0)) == ((2, 3), Machine(locked=true, 3, 2)))
}


