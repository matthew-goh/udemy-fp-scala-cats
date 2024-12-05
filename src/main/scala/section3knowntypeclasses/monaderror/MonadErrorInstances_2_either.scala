package section3knowntypeclasses.monaderror

import cats._
import cats.implicits._

// given error type E,
// functor type is Either[E, *] :  * -> *
// Either[E, *] is replaced with ({ type F[A] = Either[E, A] })#F as compiler doesn't recognise it
object MonadErrorInstances_2_either {
  def eitherME[E]: MonadError[({ type F[A] = Either[E, A] })#F, E] = new MonadError[({ type F[A] = Either[E, A] })#F, E] {
    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa match {
        case Right(a) => Right(a)
        case Left(e) => f(e)
      }

    override def pure[A](x: A): Either[E, A] = ???

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }
}
