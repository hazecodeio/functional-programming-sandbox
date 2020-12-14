package ch07

import scala.language.higherKinds

/**
 * Note that, unlike [[MonoidFoldable]]:
 *    - From this that the recursive operation takes two arguments and returns a result that becomes the first argument for the next iteration.
 *    - This observation leads to the conclusion that the folding function does not need to have both arguments of the same type.
 *    - As long as the return type is the same as the type of the first argument and the same as the type of the identity element.
 * @tparam F
 */
trait Foldable[F[_]] {
  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A,B : Monoid](as: F[A])(f: A => B): B = {
    val m = implicitly[Monoid[B]]
    foldLeft(as)(m.identity)((b, a) => m.op(f(a), b))
  }
}
