import cats._

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

// create a class Secret that overwrites the usual toString method with a hashed string
// (to prevent leaking sensitive info)
class Secret[A](val value: A) {
  private def hashed: String = { // use a known hashing algorithm
    val s = value.toString
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    val d = MessageDigest.getInstance("SHA-1")
    val hashBytes = d.digest(bytes)
    new String(hashBytes, StandardCharsets.UTF_8)
  }

  override def toString: String = hashed
}

object Secret {
  // provide the instance of Functor
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] =
      new Secret(f(fa.value))
  }
}

// generalise the modification of Secret: apply a function (f: A => B) to convert a Secret[A] to a Secret[B]
def map[A, B](secret: Secret[A])(f: A => B): Secret[B] =
  new Secret(f(secret.value))

def toUpper(name: Secret[String]): Secret[String] =
  map(name)(_.toUpperCase)

def toLower(name: Secret[String]): Secret[String] =
  map(name)(_.toLowerCase)

// This map function comes from a type class called Functor:
//trait Functor[F[_]] {
//  def map[A, B](fa: F[A])(f: A => B): F[B]
//}

// Laws:
// Identity: new Secret("hello").map(x => x) <-> new Secret("hello")
// Composition: new Secret("Hello").map(x => toLower(x)).map(x => toUpper(x)) <-> new Secret("Hello").map(x => toUpper(toLower(x)))

val leandroSecret: Secret[String] = new Secret("leandro")
leandroSecret.value

// finds the instance of Functor[Secret] in implicit scope
val upperLeandroSecret = Functor[Secret].map(leandroSecret)(_.toUpperCase)
upperLeandroSecret.value

val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
    fa match {
      case Some(x) => Some(f(x))
      case None => None
    }
  }
}

val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] = {
    fa match {
      case Nil => Nil
      case hd :: tl => f(hd) :: map(tl)(f)
    }
  }
}

// cats already provides instances of Functor[Option] and Functor[List],
// so call our own instances explicitly
optionFunctor.map(Some(3))(_ + 1)
listFunctor.map(List(1, 2, 3))(_ * 2)

// as() replaces all elements with the given one
optionFunctor.as(Some(4), 11)
listFunctor.as(List(1, 3, 5), 10)
