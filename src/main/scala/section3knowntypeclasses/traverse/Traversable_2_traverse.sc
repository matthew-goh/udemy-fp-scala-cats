import cats._
import cats.implicits._
import cats.data._

// Traverse[F[_]] extends Foldable and Functor

// traverse(fa)(f) will traverse the F[A], converting each element from A to G[B],
// then "combine" all the G[B]s into a G[F[B]]
// e.g. F is a list, G is an Option
//   def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

trait MList[+A]

object MList {
  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil extends MList[Nothing]

  def apply[A](elems: A*) = {
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))
  }

  def mnil[A]: MList[A] = MNil
  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  // 1. Write a functor instance for MList
  // 2. Implement traverse in terms of sequence and using the functor
  implicit val listFunctor: Functor[MList] = new Functor[MList] {
    override def map[A, B](fa: MList[A])(f: A => B): MList[B] = {
      fa match {
        case MNil => MNil
        case MCons(h, t) => mcons(f(h), map(t)(f))
      }
    }
  }

  implicit val listTraverse: Traverse[MList] = new Traverse[MList] {
    override def traverse[G[_], A, B](fa: MList[A])(f: A => G[B])(implicit G: Applicative[G]): G[MList[B]] = {
      fa match {
        case MNil => Applicative[G].pure(MNil)
        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
      }

      // first map all F[A] to F[G[B]], then swap to G[F[B]]
//      sequence(fa.map(f))
    }

    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] =
        as match {
          case MNil    => lb
          case MCons(h, t) => f(h, Eval.defer(loop(t)))
        }
      Eval.defer(loop(fa))
    }

    override def sequence[G[_], A](fga: MList[G[A]])(implicit G: Applicative[G]): G[MList[A]] =
      traverse(fga)(identity)
  }
}

Traverse[MList].sequence(MList(Option(5), Option(4))) // Some(MCons(5,MCons(4,MNil)))
Traverse[MList].sequence(MList(Option(5), None)) // None

Traverse[MList].traverse(MList(1,2,3))(i => Option(i+1)) // Some(MCons(2,MCons(3,MCons(4,MNil))))
