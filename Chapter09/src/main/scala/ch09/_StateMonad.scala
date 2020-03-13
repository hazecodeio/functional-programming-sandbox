package ch09

import scala.util.Random

/**
 * Added by me just to practice.
 * Bringing all related to a StateMonad into one place
 */
object _StateMonad {

  /**
   *
   * @param run State initializer??
   *            Terminal Operation that will trigger intermediate operations of map() and flatMap()?
   * @tparam S
   * @tparam A
   */
  case class State[S, A](run: S => (A, S))

  /**
   * Companion Objects:
   *    - Adding a few extra methods
   */
  object State {
    // this is actually lifting. It will be used in the StateMonad's unit()
    def apply[S, A](a: => A): State[S, A] = State(s => (a, s))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: => S): State[S, Unit] = State(_ => ((), s))
  }

  implicit def stateMonad[S] = new Monad[({type T[A] = State[S, A]})#T] {
    override def unit[B](a: => B): State[S, B] = State(a)

    override def flatMap[A, B](a: State[S, A])(f: A => State[S, B]): State[S, B] = State {
      //this will emit "S => (S, B)"
      s0 => {
        val (a1, s1) = a.run(s0)
        f(a1).run(s1)
      }
    }

  }

  /**
   * Implicit Class to add Monadic methods into Containers/Effects/TypeClasses, such as:
   * State, Option, Try, Either, etc
   *
   * @param value This will be the State[S]
   * @param monad$F$0
   * @tparam A
   * @tparam F State
   */
  implicit class MonadF[A, F[_] : Monad](val value: F[A]) {
    private val M = implicitly[Monad[F]]

    def unit(a: A) = M.unit(a)

    def flatMap[B](fab: A => F[B]): F[B] = M.flatMap(value)(fab)

    def map[B](fab: A => B): F[B] = M.map(value)(fab)
  }

}

object _StateMonadRunner extends App {

  import _StateMonad._

  case class RNG(seed: Int, i: Int) {
    def nextState: Int = new Random(i).nextInt(1)
  }

  type Rand[A] = State[RNG, A]

  def nextBool(n: Int): Rand[Boolean] = State {
    n % 2 == 0
  }

  def intPlusN(n: Int): Rand[Int] = State { s => (s.nextState + n, s) }

  val rngState = State[RNG, Int](1).map(_ + 1).map(_ + 3).flatMap(i => intPlusN(i + 1))
  println(rngState.run(RNG(1, 10)))

  val s = State[Int, String]((i: Int) => ("str", i + 1))
  println(s.run(2))

  val b = rngState.map(_ + 1).flatMap(i => nextBool(i + 1))
  println(b.run(RNG(1, 1)))

  def dice = State[Random, Int]((r: Random) => (r.nextInt(6) + 1, r))

  def anotherDice(s: Int) = State { (ss: Random) => (s + 2, new Random(s + 2L)) }

  val chainedStates = dice.map(_ + 3).flatMap(anotherDice)
  println(chainedStates.run(new Random(1L)))

}
