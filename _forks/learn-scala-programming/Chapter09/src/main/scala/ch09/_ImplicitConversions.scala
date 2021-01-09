package ch09
/**
 * Observations:
 *    - Implicit Class: to add the Monda's functions to each class of the type F[_]
 *      - such as: Seq, List, etc
 *    - I.e. Implicit Conversions
 */
object _ImplicitConversions {

    // implicit class MonadF[A, F[_]](val value: F[A])(implicit M: Monad[F]) { // Alternative to the following two lines
    implicit class MonadF[A, F[_] : Monad](val value: F[A]) {
      private val M = implicitly[Monad[F]]

      def unit(a: A) = M.unit(a)

      def flatMap[B](fab: A => F[B]): F[B] = M.flatMap(value)(fab)

      def map[B](fab: A => B): F[B] = M.map(value)(fab)
    }
}
