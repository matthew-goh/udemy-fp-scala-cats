import cats._

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  // specific instance of Applicative for Validated
  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](x: A): Validated[A] = Valid(x)

    // need to look inside vf and va and try to apply f to a
    override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] =
      (vf, va) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(e1), Valid(a)) => Invalid(e1)
        case (Valid(f), Invalid(e2)) => Invalid(e2)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
      }
  }
}

//trait Applicative[F[_]] extends Apply[F] { // Apply extends Functor
//  def pure[A](x: A): F[A] // wraps a value in F
//  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] // applies a function in the F context - can be used to define map2
//}

// Laws:
// applicativeIdentity: F.pure((a: A) => a).ap(fa) <-> fa
// pure(id) is the identity function in the F context (where ap is function application)
// applicativeHomomorphism: F.pure(f).ap(F.pure(a)) <-> F.pure(f(a))
// applicativeInterchange: ff.ap(F.pure(a)) <=> F.pure((f: A => B) => f(a)).ap(ff)
// applicativeComposition: F.pure(compose).ap(fbc).ap(fab).ap(fa) <-> fbc.ap(fab.ap(fa))
// where val compose: (B => C) => (A => B) => (A => C) = _.compose
