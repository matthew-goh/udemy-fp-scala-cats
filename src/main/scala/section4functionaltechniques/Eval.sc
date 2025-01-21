import cats._
import cats.implicits._
import cats.data._

Eval.now {
  println("Calculating...")
  5
}

val x = Eval.later {
  println("Calculating...")
  5
}
x.value // prints here
x.value // but not here

val y = Eval.always {
  println("Calculating...")
  5
}
y.value // prints here
y.value // and here

// used to handle an Eval.now() received from somewhere
val z = Eval.defer {
  println("Calculating...")
  Eval.now(5)
}
z.value // prints here
z.value // and here (no caching)

val a = Eval.later(5)
a.flatMap(i => Eval.now(i + 2)).value

object X {
  def isEven(n: Int): Eval[Boolean] =
    if(n == 0) Eval.now(true)
    else Eval.defer(isOdd(n-1))
  // returns an Eval right here, doesn't add a function call to the stack

  def isOdd(n: Int): Eval[Boolean] =
    if(n == 0) Eval.now(false)
    else Eval.defer(isEven(n-1))

  def fact(n: Int): Eval[Int] = {
    if (n <= 0) Eval.now(1)
    else Eval.defer(fact(n-1).map(_ * n))
    // multiply by n inside the Eval that is fact(n-1)
  }
}

import X._
isEven(100000).value
fact(100000).value
