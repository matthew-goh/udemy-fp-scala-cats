import cats._

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](x: A): Validated[A] = Valid(x)

    override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = {
      // NOTE: We need to use this implementation to test map2 implemented in terms of ap
      (vf, va) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(e1), Valid(a)) => Invalid(e1)
        case (Valid(f), Invalid(e2)) => Invalid(e2)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
      }
      //map2(vf, va)((f, a) => f(a))
    }

    override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = {
//      (va, vb) match {
//        case (Valid(a), Valid(b)) => Valid(f(a, b))
//        case (Invalid(e1), Valid(b)) => Invalid(e1)
//        case (Valid(a), Invalid(e2)) => Invalid(e2)
//        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
//      }

      // first step is to lift the function f, i.e. wrap it in a Validated via pure
      // but we can't work with (A, B)
      // so we need to take one argument at a time

      //val g: A => B => C = { a => { b => f(a, b) } } // f.curried
      // ap(pure(g))(va) returns Valid(g(a)), i.e. Valid(b => f(a, b))
      // so we need ap(ap(pure(g))(va))(vb)
      ap(ap(pure(f.curried))(va))(vb)
    }
  }
}
