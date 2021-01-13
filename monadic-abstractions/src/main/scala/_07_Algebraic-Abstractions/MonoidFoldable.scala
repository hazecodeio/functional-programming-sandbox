package _07_Algebraic-Abstractions

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

/**
 * Def: A type class that is capable of folding collections of type F with elements of type A
 *
 * @tparam A
 * @tparam F
 */
trait MonoidFoldable[A, F[_]] {

  def foldRight(as: F[A]): A

  def foldLeft(as: F[A]): A

  def foldBalanced(as: F[A]): A

  def foldPar(as: F[A])(implicit ec: ExecutionContext): Future[A]
}

object MonoidFoldable {

  /**
   * Observation:
   *    - Use of ContextBounds
   *    - Placing an Implicit Conversion implementing MonoidFoldable
   *    - "Type A" must be a monoid, too!
   *
   * Alternative:
   *    - Via ImplicitParam: implicit def listMonoidFoldable[A](implicit ma: Monoid[A]): MonoidFoldable[A, List]
   *
   * @tparam A
   * @return
   */
  implicit def listMonoidFoldable[A: Monoid]: MonoidFoldable[A, List] = new MonoidFoldable[A, List] {

    private val m = implicitly[Monoid[A]]

    override def foldRight(as: List[A]): A = as.foldRight(m.identity)(m.op)

    override def foldLeft(as: List[A]): A = as.foldLeft(m.identity)(m.op)

    override def foldBalanced(as: List[A]): A = as match {
      case Nil => m.identity
      case List(one) => one
      case _ => val (l, r) = as.splitAt(as.length / 2) // Multi-line case
        m.op(foldBalanced(l), foldBalanced(r)) // Notice the recursion!
    }

    private val parallelLimit = 10

    override def foldPar(as: List[A])(implicit ec: ExecutionContext): Future[A] = {
      if (as.length < parallelLimit) Future(foldBalanced(as)) // If length is less than limit, in a separate thread, call the sequential method.
      else {
        val (l, r) = as.splitAt(as.length / 2)
        Future.reduceLeft(List(foldPar(l), foldPar(r)))(m.op)
      }
    }
  }
}
