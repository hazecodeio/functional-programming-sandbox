package ch04

/**
 * Link: https://stackoverflow.com/questions/29731579/understanding-multiple-context-bounds
 */
object ContextBounds {

  trait CanEqual[T] {
    def hash(t: T): Int
  }

  implicit val stringEqual: CanEqual[String] = new CanEqual[String] {
    def hash(in: String): Int = in.hashCode()
  }
  // This is exactly the same as the previous
  //implicit val stringEqual: CanEqual[String] = (in: String) => in.hashCode

  implicit val intEqual: CanEqual[Int] = identity _

  def equal[CA, CB](a: CA, b: CB)(implicit ca: CanEqual[CA], cb: CanEqual[CB]): Boolean =
    ca.hash(a) == cb.hash(b)

  // This is equivalent to the previous method
  def equalBounds[CA: CanEqual, CB: CanEqual](a: CA, b: CB): Boolean = {
    val hashA = implicitly[CanEqual[CA]].hash(a)
    val hashB = implicitly[CanEqual[CB]].hash(b)
    hashA == hashB
  }

  // This is also equivalent to the previous method and the one before
  def equalDelegate[CA: CanEqual, CB: CanEqual](a: CA, b: CB): Boolean = equal(a, b)

  equal(10, 20)
  equalBounds("10", "20")
  equalDelegate(1598, "20")
}
