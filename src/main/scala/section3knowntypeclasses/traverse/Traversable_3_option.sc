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
    override def map[A, B](fa: MList[A])(f: A => B): MList[B] =
      fa match {
        case MNil => MNil
        case MCons(h, t) => MCons(f(h), map(t)(f))
      }
  }

  implicit val listTraverse: Traverse[MList] = new Traverse[MList] {
    override def traverse[G[_], A, B](fa: MList[A])(f: A => G[B])(implicit G: Applicative[G]): G[MList[B]] = {
      fa match {
        case MNil => Applicative[G].pure(MNil)
        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
      }
      // sequence(fa.map(f))
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

Traverse[MList].sequence(MList(Option(5), Option(4)))
Traverse[MList].sequence(MList(Option(5), None))
Traverse[MList].traverse(MList(1, 2, 3))(i => Option(i + 1))

val optionTraverse: Traverse[Option] = new Traverse[Option] {
  override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
    fa match {
      case None => G.pure(None)
      case Some(a) => f(a).map(Some.apply) // f(a) is a G[B], must map the inside to an Option[B]
    }
  }

  override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
    case None => b // f(b, "None") = b
    case Some(a) => f(b, a)
  }

  override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
    case None => lb
    case Some(a) => f(a, lb)
  }
}

optionTraverse.traverse(Some(5))(x => List(x+1, x+2)) // List(Some(6), Some(7))
optionTraverse.traverse(Some(5))(_ => List()) // List()
optionTraverse.traverse[List, Int, Int](None)(x => List(x+1, x+2)) // List(None)
