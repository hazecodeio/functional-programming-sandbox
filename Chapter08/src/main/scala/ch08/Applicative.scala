package ch08

import scala.language.{higherKinds, reflectiveCalls}
import scala.util.{Failure, Success, Try}

/**
 * Title: Applicative Functor
 *  - Works on multiple "independent" Effects Vs. Monad that works on multiple "dependent" Effects and adds the flatten()/flatMap() methods
 *    - Applicative Functors are used in parallel executions
 *    - Monads have sequential effect; i.e. blocking
 *  - Apply a pure n-ary function to n Effects
 *  - more like tupling the effects <- maybe this is the Traversables
 *    - (F[A], F[B]) => F[(A, B)]
 *    - inverted inside out
 *
 *  - Links:
 *    - https://www.youtube.com/watch?v=NVlFZYxgXDw <- [Functional programming, chapter 8. Applicative functors and profunctors. Part 1: Practical examples]
 *
 * @tparam F
 */
trait Applicative[F[_]] extends Functor[F] {
  /**
   * Def:
   *    - The apply method takes an effect, a, and a function, f,
   *      - defined in the context of the same effect and applies f to a,
   *      - returning the result that's wrapped in the very same effect.
   *    - This is a primitive method of the Applicative Functor
   *
   * @param a Parametrised Container/Structure/Effect such as Option, Try, Future, etc
   * @param f Parametrized Functions/Lambdas that will be applied on the items in the respective parametrized container/structure/effect
   * @tparam A
   * @tparam B
   * @return
   */
  def apply[A, B](a: F[A])(f: F[A => B]): F[B]

  /**
   * Def:
   *    - The unit method allows to wrap a plain value, a, into the effect.
   *      - Note that "a" is a CallByName, hence, this could be a value, block, function, etc.
   *    - This is called "Lifting"
   *
   * @param a
   * @tparam B
   * @return
   */
  def unit[B](a: => B): F[B]

  /**
   *
   * @param fa
   * @param f
   * @tparam A
   * @tparam B
   * @return
   */
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(unit(f))

  /**
   * It turned out to be a definition for the map2 function,
   * just extended with one more call for an apply for a third parameter!
   *
   * NIt is possible to implement the **mapN** method for any arity like this.
   * We can also define it in an inductive way by calling a map of smaller arity
   *
   */
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(fb)(map(fa)(f.curried))

  //    apply(fb)(apply(fa)(unit(f.curried))) // An alternative via primitives apply() + unit()

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(fc)(apply(fb)(apply(fa)(unit(f.curried)))) // Via primitives apply() + unit()

    // Alternative. Notice the currying of last param
    //    val curryF: (A, B) => C => D  = (a,b) => d => f(a,b,d)
    //    apply(fc)(map2(fa,fb)(curryF))
  }

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    // Notice the currying of the last param
    val ff: (A, B, C) => D => E = (a, b, c) => d => f(a, b, c, d)
    apply(fd)(map3(fa, fb, fc)(ff))

    // An alternative via primitives apply() + unit()
    //    apply(fd)(apply(fc)(apply(fb)(apply(fa)(unit(f.curried)))))
  }

  // Added by me
  def map5[A, B, C, D, E, Z](fa: F[A],
                             fb: F[B],
                             fc: F[C],
                             fd: F[D],
                             fe: F[E], f: (A, B, C, D, E) => Z): F[Z] = {
    // via primitives apply() + unit()
//    apply(fe)(apply(fd)(apply(fc)(apply(fb)(apply(fa)(unit(f.curried))))))

    // Notice the currying of the last param
    val ff:(A, B, C, D) => E => Z = (a,b,c,d) => e => f(a,b,c,d,e)
    apply(fe)(map4(fa,fb,fc,fd)(ff))
  }

  def product[G[_]](otherApp: Applicative[G]): Applicative[({type TupledApp[X] = (F[X], G[X])})#TupledApp] = {
    val thisApp = this
    new Applicative[({type TupledApp[X] = (F[X], G[X])})#TupledApp] {

      def unit[B](a: => B) =
        ( // A tuple of the two Applicatives' unit()
          thisApp.unit(a),
          otherApp.unit(a)
        )

      override def apply[A, B](tupledIn: (F[A], G[A]))(tupledFs: (F[A => B], G[A => B])) =
        ( //A tuple of the application of the two Applicatives via their respective apply()
          thisApp.apply(tupledIn._1)(tupledFs._1),
          otherApp.apply(tupledIn._2)(tupledFs._2)
        )
    }
  }

  // Added by me
  // This is how to compose functions
  def composeF1[A, B, C]: (B => C) => ((A => B) => (A => C)) = (f: B => C) => f.compose(_) // This is because the input to the lambda is a lambda hence the compose() is recognized
  // Fully expanded
  def composeF2[A, B, C]: (B => C) => ((A => B) => (A => C)) = bc => (ab => (a => bc.compose(ab)(a)))
  //Equivalent to the previous but using method style with currying
  def composeF3[A, B, C](bc: B => C)(ab:A => B):A => C = a => bc.compose(ab)(a) // "composeF3 _" will yield the exact same curried form
  def composeF4[A, B, C](bc: B => C)(ab:A => B)(a: A ) = bc.compose(ab)(a) // "composeF4 _" will yield the exact same curried form

  def compose[G[_]](OtherApp: Applicative[G]): Applicative[({type ComposedApp[X] = F[G[X]]})#ComposedApp] = {
    val thisApp = this

    /*
      ToDo
       - Is this an "f" of "a" and "b" as in "f(a,b)"? Or "f" of "(a => b)"?
          - It seems this "f" of "(a => b)"
     */
    def fab[A, B]: G[A => B] => (G[A] => G[B]) = (gf: G[A => B]) => (ga: G[A]) => OtherApp.apply(ga)(gf)

    /*
      ToDo
       - Is this an "f" of "g" as in "f(g)"?
          - Looks like Yes "f(g)"
     */
    def fg[A, B](f: F[G[A => B]]): F[G[A] => G[B]] = thisApp.map(f)(fab)

    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[B](a: => B) = thisApp.unit(OtherApp.unit(a)) // Notice the composition on the units

      override def apply[A, B](a: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
        thisApp.apply(a)(fg(f)) // Notice the composition on the Fs
    }
  }
}

/**
 * Concrete Applicative Functors implementing the Applicative interface
 */
object Applicative {
  implicit val bucketApplicative: Applicative[List] = new Applicative[List] {

    override def apply[A, B](a: List[A])(f: List[A => B]): List[B] = (a, f) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (aa :: as, ff :: fs) => // Multi-line statements of a return case

        val fab: (A => B) => B = f => f(aa)

        /**
         * - Notice the chained application of an "ff" on its corresponding "aa".
         * - Also, Notice the recursive call of "apply()"
         * - "fs.map(fab)" is expecting "(A => B)" as in input the reason why "fab" the way it is.
         *    - Alternatively, this could be rewritten as "fs.map(f => f(aa))" and no need for "fab"
         */
        ff(aa) :: as.map(ff) ::: fs.map(fab) ::: apply(as)(fs)
      case _ => Nil
    }

    override def unit[A](a: => A): List[A] = List(a)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def apply[A, B](a: Option[A])(f: Option[A => B]): Option[B] = (a, f) match {
      case (Some(a), Some(f)) => Some(f(a))
      case _ => None
    }

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  implicit def eitherApplicative[L] = new Applicative[({type T[A] = Either[L, A]})#T] {
    override def apply[A, B](a: Either[L, A])(f: Either[L, A => B]): Either[L, B] = (a, f) match {
      case (Right(a), Right(f)) => Right(f(a))
      case (Left(l), _) => Left(l)
      case (_, Left(l)) => Left(l)
    }

    override def unit[A](a: => A): Either[L, A] = Right(a)
  }

  implicit val tryApplicative: Applicative[Try] = new Applicative[Try] {
    override def apply[A, B](a: Try[A])(f: Try[A => B]): Try[B] = (a, f) match {
      case (Success(a), Success(f)) => Try(f(a))
      case (Failure(ex), _) => Failure(ex)
      case (_, Failure(ex)) => Failure(ex)
    }

    override def unit[A](a: => A): Try[A] = Success(a)
  }


}
