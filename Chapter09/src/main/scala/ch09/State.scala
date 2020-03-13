package ch09

import scala.language.higherKinds

/**
 * Link:
 *    - https://www.youtube.com/watch?v=Pgo73GfHk0U
 *
 * @param run
 */
final case class State[S, A](run: S => (A, S)) {
  /**
   * This also matches the signature of "flatMap[B](f: A => F[B]): F[B]"!!
   *    - The implicit "StateMonad" will add "map()" and "flatMap()"
   *    - Replace "F" with "State"
   *      - def flatMap[B](f:A => State[S, B]): State[S, B]
   */
  def compose[B](f: A => State[S, B]): State[S, B] = {
    /**
     * It matches "run: S => (A, S)"
     */
    val composedRuns = (currentState: S) => { // return type "S => (B, S)'
      val (a, nextState) = run(currentState)
      f(a).run(nextState)
    }
    State(composedRuns)
  }

  /**
   * Observations:
   *  - Added by me
   *  - after following: https://www.youtube.com/watch?v=Pgo73GfHk0U
   *  - This is actually already part of MonadF in "lowPriorityImplicits"
   */
  /*def map[B](f: A => B): State[S, B] = State {
    s0 => {
      val (a, s1) = run(s0)
      (f(a), s1) // This is where it's different from flatMap()
    }
  }*/

  /**
   * Observations:
   *  - Added by me
   *  - after following: https://www.youtube.com/watch?v=Pgo73GfHk0U
   *  - This is actually already part of MonadF in "lowPriorityImplicits"
   */
  /*def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s0 => {
      val (a, s1) = run(s0)
      f(a).run(s1) // This is where it's different from map()
    }
  }*/
}

object State {
  def apply[S, A](a: => A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: => S): State[S, Unit] = State(_ => ((), s))
}

/**
 * Added by me
 *
 * a common pattern:
 * each of our functions has a type of the form RNG => (A, RNG) for some type A.
 * Functions of this type are called state actions or state transitions because they transform RNG states from one to the next.
 * These state actions can be combined using combinators, which are higher-order functions that we’ll define in this section.
 * Since it’s pretty tedious and repetitive to pass the state along ourselves, we want our combinators to pass the state from one action to the next automatically.
 *
 * type Rand[+A] = RNG => (A, RNG)
 *
 * a state action—a program that depends on some RNG, uses it to generate an A, and also transitions the RNG to a new state that can be used by another action later.
 */
object RNGExample extends App {

  type Seed = Long

  def rng(seed: Seed): (Seed, Long) = (1L, 1L)

  // import the implicit "map, flatMap, and unit"
  import Monad.lowPriorityImplicits._

  def nextLong(seed: Seed): State[Seed, Long] = State(seed => rng(seed))// same as "State(rng(_))"

  def nextBool(seed: Seed): State[Seed, Boolean] = nextLong(seed).map(_ > 0L) // After adding/importing "map()", now we can map other States using existing ones

  val aa = for {
    a <- nextLong(1L)
  } yield a

//  println(aa.run(1L)._2)
//  println(State[Seed, Long](1L).map(nextLong(_)))
//  println(nextLong(1L).run(1L)._2)

  val bb = for {
    a <- nextLong(1L)
    b <- nextBool(a)
  } yield (a, b)
  println(bb)
//  println(bb.run(1L)._2)
//  println(State[Seed, Long](1L).flatMap(nextLong(_)).flatMap(nextBool(_)))


}

object StateExample extends App {

  lazy val consumption = 1f

  /**
   * Float: State
   * Boat: Result
   */
  type FuelState = State[Float, Boat] // State[S, A]

  //ToDo - How about making FuelConsumption part of Boat class?
  def consume(speed: Float, time: Float) = consumption * time * speed

  /**
   *
   * @param boat more like a delegate object
   * @return FuelState
   */
  def go(speed: Float, time: Float)(boat: Boat): FuelState = new State((fuel: Float) => {
    val newFuel = fuel - consume(speed, time)
    (boat.go(speed, time), newFuel) // S => (A, S) // Float => (Boat, Float)
  })

  /**
   *
   * @param boat more like a delegate object
   * @return FuelState
   */
  def turn(angle: Double)(boat: Boat): FuelState = State(boat.turn(angle))

  import Boat.boat

  println(boat.go(10, 5).turn(0.5).go(20, 20).turn(-0.1).go(1, 1))

  import Monad.lowPriorityImplicits._

  def move(boat: Boat) = State[Float, Boat](boat). // this is the CompanionObject's "apply[S, A](a: => A): State[S, A]"
    flatMap(go(10, 5)). // flatMap is recognized from the "implicit class MonadF"
    flatMap(turn(0.5)).
    flatMap(go(20, 20)).
    flatMap(turn(-0.1)).
    flatMap { b: Boat => go(1, 1)(b) }

  def mv(boat: Boat) = for {
    a <- State[Float, Boat](boat)
    f1 <- State.get[Float]
    _ = logFuelState(f1)
    _ <- State.set(Math.min(700, f1))
    b <- go(10, 5)(a)
    f2 <- State.get[Float]; _ = logFuelState(f2)
    c <- turn(0.5)(b)
    f3 <- State.get[Float]; _ = logFuelState(f3)
    d <- go(20, 20)(c)
    f3 <- State.get[Float]; _ = logFuelState(f3)
    e <- turn(-0.1)(d)
    f3 <- State.get[Float]; _ = logFuelState(f3)
    f <- go(1, 1)(e)
  } yield f

  println(move(boat).run(1000f))
  println(mv(boat).run(1000f))

  def logFuelState(f: Float) = println(s"Current fuel level is $f")
}


object PolymorphicStateExample1 extends App {

  import StateExample.consume

  type FuelStateBoat = State[Float, Boat]

  def go[M[_] : Monad](speed: Float, time: Float)(boat: Boat): FuelStateBoat = new State((fuel: Float) => {
    val newFuel = fuel - consume(speed, time)
    (boat.go(speed, time), newFuel)
  })

  def turn(angle: Double)(boat: Boat): FuelStateBoat = State(boat.turn(angle))

  import Monad.stateMonad
  import Monad.lowPriorityImplicits._

  def move(boat: Boat): State[Float, Boat] = for {
    a <- State[Float, Boat](boat)
    i <- State.get[Float]
    _ <- State.set(Math.min(700, i))
    b <- go(10, 5)(a)
    c <- turn(0.5)(b)
    d <- go(20, 20)(c)
    e <- turn(-0.1)(d)
    f <- go(1, 1)(e)
  } yield f

  println(move(Boat.boat).run(1000f))
}

object PolymorphicStateExample2 extends App {

  import StateExample.consume

  def go(speed: Float, time: Float)(boat: Boat): State[Float, Boat] = new State((fuel: Float) => {
    val newFuel = fuel - consume(speed, time)
    (boat.go(speed, time), newFuel)
  })

  import Monad.stateMonad
  import Boat.{move, turn, boat}

  type FuelState[B] = State[Float, B]
  println(move(go, turn[FuelState])(State(boat)).run(1000f))
}
