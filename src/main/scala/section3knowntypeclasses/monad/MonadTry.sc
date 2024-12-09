import cats._
import cats.implicits._
import scala.util._

// trait Monad[F[_]] extends ... {
// def pure[A](x: A): F[A]
// def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
// }

// a controversial Monad - it can break one of the laws
// should have pure(x).flatMap(f) === f(x)
// this example is ok
val f: Int => Try[Int] = { i => Success(i+1) }
Success(10).flatMap(f)
f(10)

// but here, since Scala captures the error and wraps it in a Failure...
val f2: Int => Try[Int] = { i => throw new Exception("boom") }
Success(10).flatMap(f2) // Failure(java.lang.Exception: boom)
//           f2(10) // exception with stack trace, program stops running

implicit val tryMonad: Monad[Try] = new Monad[Try] {
  override def pure[A](x: A): Try[A] = Success(x)

  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
    fa match {
      case Success(a) => f(a)
      case Failure(e) => Failure(e)
    }

  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
}

tryMonad.pure(5)
tryMonad.pure(5).flatMap(i => tryMonad.pure(i+1))
tryMonad.pure(5).flatMap(i => Failure(new Exception("boom")))
tryMonad.pure(5)
  .flatMap((i: Int) => Failure(new Exception("boom"))
    .flatMap((j: Int) => Failure(new Exception("boom2"))))