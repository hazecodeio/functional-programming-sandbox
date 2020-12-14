package ch02

object GeneralisedConstraints {
  import Linearization._
  abstract class Wrapper[A] {
    val a: A

    // A in flatten shadows A in Wrapper
    // def flatten[B, A <: Wrapper[B]]: Wrapper[B] = a
    def flatten(implicit ev: A <:< Wrapper[B]): Wrapper[B] = a
  }

  /**
   * Another example by me
   */
  class Container[A](value: A) {
    //def diff[A <: Int](b: Int) = value - b
    def diff(b: Int)(implicit env: A <:< Int) = value - b
  }

  /**
   * Another example by me
   */
  def foo[A, B <: A](a: A, b: B) = (a, b)// A is inferred as a supertype to B by "B <: A"! The common supertype between Int and List is Any!

  /*
   * Because generalized type constraints does not interfere with inference, A is Int here.
   * Only then does the compiler find evidence for <:<[Int, List[Int]] and then fails.
   */
  def bar[A,B](a: A, b: B)(implicit ev: B <:< A) = (a, b)// The implicit with "<:<" is restricting/inferring correctly!!
}
