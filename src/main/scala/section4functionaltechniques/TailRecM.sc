import cats._
import cats.implicits._

// tailRecM encodes the idea of safe recursion

val optionMonad: Monad[Option] = new Monad[Option] {
  override def pure[A](x: A): Option[A] = Some(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
    fa match {
      case Some(a) => f(a)
      case None => None
    }

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = {
    f(a) match {
      case Some(Right(b)) => Some(b)
      case Some(Left(a)) => tailRecM(a)(f)
      case None => None
    }
  }
}

// p is a predicate that controls whether to keep iterating (if true) or return a value (if false)
def iterateWhileM[A](initial: A)(f: A => Option[A])(p: A => Boolean): Option[A] =
  if (p(initial)) f(initial).flatMap(a => iterateWhileM(a)(f)(p))
  else Some(initial)

iterateWhileM(1)(n => Some(n+1))(_ < 5) // Some(5)
//iterateWhileM(1)(n => Some(n+1))(_ < 100000) // StackOverflowError

// using tailRecM, note that B is the return type
def safeIterateWhileM[A](initial: A)(f: A => Option[A])(p: A => Boolean): Option[A] =
  optionMonad.tailRecM(initial){ a =>
    if (p(a)) f(a).map(Left(_)) // if we have to keep iterating, use the Left
    else Some(Right(a))
  }

safeIterateWhileM(1)(n => Some(n+1))(_ < 100000)
