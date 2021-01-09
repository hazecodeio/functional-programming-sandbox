package ch08

/**
 * Observations:
 *    - Implicit Class: to add the Functor's functions to each class of the type F[_]
 *      - such as: Seq, List, etc
 *    - I.e. Implicit Conversions
 */
object _ImplicitConversions {

  // implicit class FunctorF[A, F[_]](val value: F[A])(implicit M: Functor[F]) { // Alternative to the following two lines
  implicit class FunctorF[A, F[_] : Functor](val value: F[A]) {
    private val M = implicitly[Functor[F]]
    def map[B](fab: A => B): F[B] = M.map(value)(fab)
  }

  implicit class ApplicativeF[A, F[_] : Applicative](val value: F[A]) {
    private val M = implicitly[Applicative[F]]
    def map[B](fab: A => B): F[B] = M.map(value)(fab)
    def apply[B](fab: F[A => B]): F[B] = M.apply(value)(fab)

    def map2[B, C](b: F[B])(f: (A, B) => C): F[C] = M.map2(value, b)(f)

    def map3[B, C, D](   fb: F[B],
                         fc: F[C])(f: (A, B, C) => D): F[D] = M.map3(value, fb, fc)(f)

    def map4[B, C, D, E](   fb: F[B],
                            fc: F[C],
                            fd: F[D])(f: (A, B, C, D) => E): F[E] = M.map4(value, fb, fc, fd)(f)

    // Added by me
    def map5[B, C, D, E, Z](   fb: F[B],
                               fc: F[C],
                               fd: F[D],
                               fe: F[E], f: (A, B, C, D, E) => Z): F[Z] = M.map5(value, fb, fc, fd, fe)(f)
  }
}