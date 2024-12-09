import cats._
import cats.implicits._

//trait Foldable[F[_]] {
//  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
//  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
//  def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B
//}
// for folding: A is the list type, B is the default and return value type
// Eval defers computation of the recursive call until needed -> lazy

// foldMap applies f to all elements and combines them all with the Monoid combine
// e.g. foldMap[Int, String](List(1,2,3))(_.show) === "123"
//def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B =
//  foldLeft(fa, M.empty)((monoidAcc, a) => M.combine(monoidAcc, f(a)))

// Laws:
// leftFoldConsistentWithFoldMap: fa.foldMap(f) <-> fa.foldLeft(M.empty) {(b,a) => b |+| f(a)}
// rightFoldConsistentWithFoldMap: fa.foldMap(f) <-> fa.foldRight(Later(M.empty)) {(a,lb) => lb.map(f(a) |+| _)}.value

trait MList[+A]

object MList {
  // use FoldRight to construct a list
  // A* is treated as an Array[A]
  def apply[A](elems: A*): MList[A] =
    elems.foldRight(mnil[A])((elem, curTail) => mcons(elem, curTail))

  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil extends MList[Nothing]

  def mnil[A]: MList[A] = MNil
  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val listFoldable: Foldable[MList] = new Foldable[MList] {
    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] =
        as match {
          case MNil => lb
          case MCons(h, t) => f(h, Eval.defer(loop(t)))
        }
      Eval.defer(loop(fa))
    }
  }
}

import MList._
// using apply method
MList(1, 2, 3)

def sum(ints: MList[Int]): Int =
  ints.foldLeft(0)((b, a) => b + a)
//  Foldable[MList].foldLeft(ints, 0)((b, a) => b + a)

def length[A](list: MList[A]): Int =
  list.foldLeft(0)((b, a) => b + 1)

def filterPositive(ints: MList[Int]): MList[Int] =
  ints.foldRight(Eval.now(mnil[Int]))((i, eis) => if(i > 0) Eval.now(mcons(i, eis.value)) else eis).value
  // ints.foldLeft(mnil[Int])((b, a) => if(a > 0) mcons(a, b) else b) // order of elements reversed!
//now() gives eager evaluation

sum(MList(1,2,3))
length(MList(1,2,3,4,5))
filterPositive(MList(-1,0,1,2))

// String Monoid has concatenation combine method
// show is the function converting Int to String
MList(1,2,3).foldMap(_.show)
// Int Monoid has addition combine method
MList(1,2,3).foldMap(_ * 2) // 2 + 4 + 6 = 12
// if the List type is a Monoid, fold will combine all elements
MList(1,2,3).fold // 1 + 2 + 3 = 6
MList("hello ", "world").fold
