package ch09

/**
 * - The State monad represents an external state
 * - It accepts an external context and passes it over unchanged to every computation down the queue
 * - Reader will have access to read-only system properties
 *
 * @param run
 * @tparam R
 * @tparam A
 */
final case class Reader[R, A](run: R => A) {
  def compose[B](f: A => Reader[R, B]): Reader[R, B] = Reader { r: R =>
    f(run(r)).run(r)
  }
}

object Reader {
  def apply[R, A](a: => A): Reader[R, A] = Reader(_ => a)
}


object ReaderExample extends App {
  // As this is external regulation, we have to model it with a case class
  final case class Limits(speed: Float, angle: Double)
  type ReaderLimits[A] = Reader[Limits, A]

  def go(speed: Float, time: Float)(boat: Boat): ReaderLimits[Boat] = Reader(limits => {
    val lowSpeed = Math.min(speed, limits.speed)
    boat.go(lowSpeed, time)
  })

  def turn(angle: Double)(boat: Boat): ReaderLimits[Boat] = Reader(limits => {
    val smallAngle = Math.min(angle, limits.angle)
    boat.turn(smallAngle)
  })

  import Monad.readerMonad
  import Boat.{move, boat}

  println(move(go, turn)(Reader(boat)).run(Limits(10f, 0.1)))
}
