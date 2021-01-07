package ch01

import java.util.concurrent.atomic.AtomicLong

import scala.io.StdIn
import scala.util.{Try, Using}

case class User(name: String, surname: String, email: String)

object Ch01 extends App {

  "10".toIntOption
  "TrUe".toBooleanOption

  val bool = "Not True"
  bool.toBooleanOption

  val user = User("John", "Doe", "jd@mail.me")
  user.productElementNames.mkString(", ")
//  user.productElementName(3) // runtime exception: java.lang.IndexOutOfBoundsException

  val tuple = (1, "two", false)

  tuple.productElementNames.mkString(", ")
  tuple.productElementName(1)

  def naiveToJsonString(p: Product): String =
    (for { i <- 0 until p.productArity } yield
      s""""${p.productElementName(i)}": "${p.productElement(i)}"""")
      .mkString("{ ", ", ", " }")

  naiveToJsonString(user)

  import scala.util.chaining._

  import UserDb._
  val userId = 1L
//  save(update(getById(userId)))

//  getById(userId).pipe(update).pipe(save)

//  val doEverything = (getById _).andThen(update).andThen(save)
//  doEverything(userId)

  val lastTick = new AtomicLong(0)
  def start(): Unit = lastTick.set(System.currentTimeMillis())
  def measure[A](a: A): Unit = {
    val now = System.currentTimeMillis()
    val before = lastTick.getAndSet(now)
    println(s"$a: ${now - before} ms elapsed")
  }

//  start()
  print("Enter: ")
  val result = StdIn.readLine().pipe(_.toIntOption).tap(measure)
  print("Enter: ")
  val anotherResult = StdIn.readLine().pipe(_.toIntOption).tap(measure)

  final case class Resource(name: String) extends AutoCloseable {
    override def close(): Unit = println(s"Closing $name")
    def lines = List(s"$name line 1", s"$name line 2")
  }
  val List(r1, r2, r3) = List("first", "2", "3").map(Resource)

  /**
   * ToDo - Needs fixing. Check ScalaDoc for Using(){this_part_is_missing}
   *  Done!
   */

  val lines: Try[Seq[String]] = for {
    u1 <- Using(r1){_.lines}
    u2 <- Using(r2){_.lines}
    u3 <- Using(r3){_.lines}
  } yield {
    u1 ++ u2 ++ u3
  }

  println(lines)

}

object UserDb {
  def getById(id: Long): User = ???
  def update(u: User): User = ???
  def save(u: User): Boolean = ???
}
