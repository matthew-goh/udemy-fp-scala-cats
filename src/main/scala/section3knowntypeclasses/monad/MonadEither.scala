package section3knowntypeclasses.monad

import cats._
import cats.implicits._

// Either has type * -> * -> *
// But Monad needs type * -> *
// To eliminate one parameter, we must fix one type in the Either
// In cats, Either is right-biased (the place to compute values), so fix the left

object MonadEither {
  // to reuse code for different possible left types, use a def and take one type parameter E
  // should be Monad[Either[E, *]], but compiler doesn't recognise it
  implicit def eitherMonad[E]: Monad[({ type F[A] = Either[E, A] })#F] = new Monad[({ type F[A] = Either[E, A] })#F] {
    override def pure[A](x: A): Either[E, A] = Right(x)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }

  // Either should be fail-fast (abort immediately when a failure is reached)
  def main(args: Array[String]): Unit = {
    val x = 5.asRight[String].flatMap(i => (i + 1).asRight[String]) // Right(6)
    // val y = 5.asRight[String].flatMap(i => "boom".asLeft[Int]) // Left("boom"): Either[String, Int] - indicate the Right type
    val y = 5.asRight[String].flatMap(i => "boom".asLeft[Int].flatMap(j => "boom 2".asLeft[Int])) // still Left("boom")
    println(x)
    println(y)
  }
}
