import cats._
import cats.implicits._

// want this behaviour (uses flatMap from standard library):
val result = for {
  a <- List(1,2,3)
  b <- List(4,5,6)
} yield a + b  // Cartesian product adding


val listMonad: Monad[List] = new Monad[List] {
  override def pure[A](x: A): List[A] = List(x)

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
    fa match {
      case hd :: tl => f(hd) ++ flatMap(tl)(f)
      case Nil => Nil
    }

  override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
}

listMonad.flatMap(List(1,2,3))(a => List(a+1, a+2))
