import scala.annotation.tailrec
// cats not needed here!

@tailrec
def fact(n: Int, acc: Int = 1): Int = {
  if (n == 0) acc
  else fact(n - 1, n * acc)
}

fact(10)

trait Trampoline[-A]
object Trampoline {
  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, B](ta: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]
  // ta is the trampoline of the recursive call, f "appends the head"

  // auxiliary function
  // left side suspends a computation for later
  def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] = ta match {
    case Done(a: A) => Right(a)
    case More(thunk) => resume(thunk())
    case FlatMap(t: Trampoline[Any], f: (Any => Trampoline[A])) => t match { // match over the trampoline
      case Done(a2) => resume(f(a2)) // apply the function to the available value
      case More(thunk2) => Left(() => FlatMap(thunk2(), f)) // consume the thunk
      case FlatMap(t2: Trampoline[Any], f2: (Any => Trampoline[Any])) =>
        resume(FlatMap(t2, (x: Any) => FlatMap(f2(x), f)))
    }
  }
  // use associativity of monads
  // FlatMap(FlatMap(t2, f2), f) --> FlatMap(t2, x => FlatMap(f2(x), f))

  @tailrec
  def run[A](ta: Trampoline[A]): A = resume(ta) match {
    case Right(a) => a
    case Left(thunk) => run(thunk())
  }
//  def run[A](ta: Trampoline[A]): A = ta match {
//    case Done(a: A) => a
//    case More(thunk) => run(thunk())
//  }
}

// cannot define mutually recursive functions in a worksheet at the top level
object X {
  import Trampoline._

  def isEven(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(true)
    else More(() => isOdd(n - 1))

  def isOdd(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(false)
    else More(() => isEven(n - 1))
}

import X._
import Trampoline._
isEven(6) // More(X$<function>)
run(isEven(1000000))

// Usual flatMap is:
//def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
//  case Nil => Nil
//  case h :: t => f(h) ::: flatMap(t)(f) /// ::: can prepend multiple elements
//}

def flatMap[A, B](as: List[A])(f: A => List[B]): Trampoline[List[B]] = as match {
  case Nil => Done(Nil)
  case h :: t => More { () =>
    FlatMap(flatMap(t)(f), (lb: List[B]) => Done(f(h) ::: lb))
    // FlatMap[Trampoline[List[B]], List[B] => Trampoline[List[B]]]
  }
}

run(flatMap((1 to 100000).toList)(i => List(i, i+1)))
