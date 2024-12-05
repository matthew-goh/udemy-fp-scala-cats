import cats._
import cats.implicits._

import java.io.IOException
import scala.util.{Failure, Success, Try}

trait HttpMethod
case object GET extends HttpMethod
case class HttpRequest(method: HttpMethod, url: String)
case class HttpResponse(status: Int)

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

val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {
  override def raiseError[A](e: Unit): Option[A] = None

  override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
    fa.orElse(f(()))

  override def pure[A](x: A): Option[A] = Some(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]) = ???
}

// to accommodate various error types, use a type parameter E
def executeRequestME[F[_], E](request: HttpRequest)(f: Exception => E)(implicit ME: MonadError[F, E]): F[HttpResponse] =
  try {
    ME.pure(doRequest(request))
  } catch {
    // e is an Exception, but that is not necessarily covered by type E, which could be anything
    // so we need to pass in another function f that converts e into a suitable object of type E
    case e: Exception => ME.raiseError(f(e))
  }

type ErrorOr[A] = Either[String, A]
executeRequestME[Option, Unit](HttpRequest(GET, "www.eeexample.com"))((e: Exception) => ())
executeRequestME[ErrorOr, String](HttpRequest(GET, "www.eeexample.com"))(e => e.getMessage)


// MonadError additional functions
// attempt(): returns the success case with an Either inside
MonadError[Option, Unit].attempt(Some(5)) // Some(Right(5))
MonadError[Option, Unit].attempt(None) // Some(Left(()))
MonadError[Try, Throwable].attempt(Failure(new Exception("boom")))

// ensure(): tests a predicate on a value and if false, returns the error case, else just returns the F(value)
// value, error and predicate must all be provided
MonadError[Option, Unit].ensure(Some(3))(())(_ % 2 == 0) // None
MonadError[ErrorOr, String].ensure(Right(5))("boom")(_ % 2 == 0) // Left(boom)
