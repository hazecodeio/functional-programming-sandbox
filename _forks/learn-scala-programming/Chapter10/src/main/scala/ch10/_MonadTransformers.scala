object _MonadTransformers {

  import ch09.Monad

  // Lift the empty option into the target Effect such as e.g. Future
  private def noResultOptionT[F[_] : Monad, B]: F[Option[B]] = Monad[F].unit(Option.empty[B])

  implicit class OptionT[F[_] : Monad, A](val value: F[Option[A]]) {
    def compose[B](f: A => OptionT[F, B]): OptionT[F, B] = {

      import ch09.Monad.lowPriorityImplicits._
      val result = value.flatMap {
        case None => noResultOptionT[F, B]
        case Some(a) => f(a).value
      }
      new OptionT(result)
    }

    def isEmpty: F[Boolean] = Monad[F].map(value)(_.isEmpty)
  }

  def optionTunit[F[_] : Monad, A](a: => A) = new OptionT(Monad[F].unit(Option(a)))

  implicit def OptionTMonad[F[_] : Monad] = new Monad[({type T[A] = OptionT[F, A]})#T] {

    override def unit[A](a: => A): OptionT[F, A] = Monad[F].unit(Monad[Option].unit(a)) // This is actually being converted by the implicit class

    override def flatMap[A, B](a: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = a.compose(f)
  }
}
