import cats._
import cats.implicits._
import cats.data._

// Traverse[F[_]] extends Foldable and Functor
// has foldLeft and foldRight from Foldable

// traverse(fa)(f) will traverse the F[A], converting each element from A to G[B],
// then "combine" all the G[B]s into a G[F[B]]
// e.g. F is a list, G is an Option
//   def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

// Laws:
// traverseIdentity: fa.traverse[Id, B](f) <-> F.map(fa)(f)
// if you traverse something using the identity applicative functor (i.e. G[B] = B), what you get is just a map
// traverseSequentialComposition:
// Nested(fa.traverse(f).map(fb => fb.traverse(g)) <-> fa.traverse(a => Nested(f(a).map(g)))
// if you do 2 traverses, i.e. map over (the result of the 1st traverse) using (another traverse with g)...
// you can actually save one traverse, by just traversing over fa with a function that is the result of f(a).map(g)
// Let f: A => M[B] and g: B => N[C], then
// fa.traverse(f): M[F[B]], LHS result: M[N[F[C]] === Nested[M, N, F[C]]
// f(a).map(g): "M[B.map(g))]" = M[N[C]], RHS result: M[N[F[C]]

trait MList[+A]

object MList {
  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil extends MList[Nothing]

  def apply[A](elems: A*) = {
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))
  }

  def mnil[A]: MList[A] = MNil
  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val listTraverse: Traverse[MList] = new Traverse[MList] {
    override def traverse[G[_], A, B](fa: MList[A])(f: A => G[B])(implicit G: Applicative[G]): G[MList[B]] = {
      fa match {
        case MNil => Applicative[G].pure(MNil)
        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
      }
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

    // sequence comes with Traverse
    // swaps the types from MList[G[A]] to G[MList[A]], e.g. list of Futures to a single Future containing a list
    override def sequence[G[_], A](fga: MList[G[A]])(implicit G: Applicative[G]): G[MList[A]] =
      traverse(fga)(identity)
  }
}
