import cats._
import cats.data._
import cats.implicits._

// State Monad
// class State[S, A](val run: S => (S, A)) {
//   ...
// }
// S is a state, A is a return value
// run is a function from initial state to (final state, return value)

//object State {
//  def get[S]: State[S, S] // reads the state
//  def set[S](s: S): State[S, Unit] // writes the state
//  def modify[S](f: S => S): State[S, Unit] // updates the state with a function
//}

// State ==~ S => (S, A)

// Call apply() method - needs a function S => (S, A)
State[Int, Double](s => (s + 1, s.toDouble * 2))

type St[A] = State[Int, A]
// when calling pure, the value provided determines A
8.pure[St] // State[Int, Int](s => (s, 8))
8.pure[St].run(1).value // (1, 8)
// instances of states are functions, don't show anything meaningful

// call state.run(initialState)
// evaluation of states is lazy, must call value
val x = State[Int, Double](s => (s + 1, s.toDouble * 3))
x.run(4).value // (5, 12.0)
x.runA(4).value // 12.0
x.runS(4).value // 5

State.get[Int].run(5).value // (5,5)
State.set[Int](10).run(1).value // (10,())
State.modify[Int](s => s * 5).run(3).value // (15,())
State.inspect[Int, Int](s => s * 3).run(2).value // (2,6)
State.inspect[Int, String](s => s.toString + "!").run(2).value // (2,2!)

def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))

// How can we compose and combine states?
// after doing run(), the map function is applied to the return value
x.map(_ * 2.5).run(2).value // 2 -> (3,6.0) -> (3,15.0)
// flatMap makes accumulated changes to the state
x.flatMap(d => State[Int, Double](s => (s*2, d+3))).run(1).value // 1 -> (2,3.0) -> (2*2, 3+3) = (4,6.0)

val y = State[Int, Double](s => (s * 2, s.toDouble / 2))
(x, y).mapN((d1, d2) => d1 + d2).run(3).value
// x.run gives (4,9.0)
// y.run on this (initial state=4) gives (8,2.0)
// result is (8,11.0) [sum the return values]

// x.flatMap(_ => y) is the same as x >> y
val t1 = State.modify[Int](s => s * 2)
val t2 = State.modify[Int](s => s * 3)
(t1 >> t2).run(10).value // (60, ())
(t1 >> t2 >> 8.pure[St]).run(10).value // (60, 8)
