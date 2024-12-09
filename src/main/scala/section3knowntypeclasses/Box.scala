package section3knowntypeclasses

import cats._
import cats.implicits._

// Box just wraps a value of any type
case class Box[A](value: A)

object Box {
  // Implement an instance of Eq[Box[A]] for any A that has an Eq instance
  // need a different instance for every A, so use def
  implicit def eqBox[A](implicit eqA: Eq[A]): Eq[Box[A]] = Eq.by(_.value)

  // Implement an instance of Monad[Box]
  implicit val monadBox: Monad[Box] = new Monad[Box] {
    override def pure[A](x: A): Box[A] = Box(x)

    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = f(fa.value)

    override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      f(a).value match {
        case Right(b) => Box(b)
        case Left(a) => tailRecM(a)(f)
      }
  }
}
