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

// we don't know what F will be, so we need an implicit parameter giving the instance of MonadError
def executeRequestME[F[_]](request: HttpRequest)(implicit ME: MonadError[F, Throwable]): F[HttpResponse] =
  try {
    ME.pure(doRequest(request)) // return Some(), Right() or Success()
  } catch {
    case e: Exception => ME.raiseError(e)
  }

type ErrorOr[A] = Either[Throwable, A]
executeRequestME[ErrorOr](HttpRequest(GET, "abc"))
// but if we try to use Option here, there is a compilation error:
// No implicits found for parameter ME: MonadError[Option, Throwable]
