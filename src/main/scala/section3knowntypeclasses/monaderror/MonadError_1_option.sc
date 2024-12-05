import cats._
import cats.implicits._

import java.io.IOException
import scala.util.{Failure, Success, Try}

trait HttpMethod
case object GET extends HttpMethod
case class HttpRequest(method: HttpMethod, url: String)
case class HttpResponse(status: Int)

// method that could throw an exception
def doRequest(req: HttpRequest): HttpResponse =
  if(math.random() < 0.5) throw new IOException("boom!")
  else HttpResponse(200)

def executeRequest(req: HttpRequest): Option[HttpResponse] =
  try {
    Some(doRequest(req))
  } catch {
    case _: Exception => None
  }

def executeRequest2(req: HttpRequest): Either[String, HttpResponse] =
  try {
    Right(doRequest(req))
  } catch {
    case _: Exception => Left("Sorry :(")
  }

def executeRequest3(req: HttpRequest): Try[HttpResponse] =
  try {
    Success(doRequest(req))
  } catch {
    case e: Exception => Failure(e)
  }

executeRequest3(HttpRequest(GET, "ww.eeexample.com"))

// The MonadError type class extends Monad but has 2 extra methods (from ApplicativeError): raiseError and handleErrorWith
//trait MonadError[F[_], E] {
//  def raiseError[A](e: E): F[A]
//  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
//}
// F[_] is the functor used for error handling, E is the type of the error
// should capture the above 3 ways of handling errors

// Laws:
// monadErrorLeftZero (fail-fast): F.flatMap(F.raiseError[A](e))(f) <-> F.raiseError[B](e)

// Option type of error handling:
// must return None when there is an error and no other info can be given, so a good error type is Unit
val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {
  override def raiseError[A](e: Unit): Option[A] = None

  // tries to recover from the error using f
  override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
    fa.orElse(f(()))
//    fa match {
//      case x @ Some(_) => x
//      case None => f(())
//    }

  override def pure[A](x: A): Option[A] = Some(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]) = ???
}
