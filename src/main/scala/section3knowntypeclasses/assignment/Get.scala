package section3knowntypeclasses.assignment

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}
import scala.util.{Failure, Success, Try}

/**
 * The Get monad parses values from a list of bytes, keeping track of the
 * remaining input after each operation.
 *
 * The run function reads and consumes the bytes needed to construct a value of A.
 * If there is any issue (i.e: insufficient input), it should signal it via a Left result.
 * If everything goes ok, it should return the remaining input along with the parsed value.
 *
 * For more information of a real implementation for Haskell, check out:
 * https://wiki.haskell.org/Dealing_with_binary_data
 */
case class Get[A](run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  /**
   * TODO 1
   * Consumes n bytes of input parsing no value.
   */
  def skip(n: Int): Get[Unit] = Get(bytes => {
    if (bytes.length < n) Left("Insufficient input")
    else Right(bytes.drop(n), ())
  })

  /**
   * TODO 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get(bytes => Right(bytes, bytes.isEmpty))

  /**
   * TODO 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get(bytes => {
    if (bytes.isEmpty) Left("Insufficient input")
    else Right(bytes.tail, bytes.head)
  })

  /**
   * TODO 4
   * Reads an Int from input using Big Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  // replicateA(n)(implicit F: Applicative[F]): Given fa and n, apply fa n times to construct an F[List[A]] value.
  // Then map from List[A = Byte] to B = Int
  def getIntBE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.BIG_ENDIAN)
  }
//  def getIntBE: Get[Int] = Get(bytes => {
//    if (bytes.length < 4) Left("Insufficient input")
//    else Try(bytesToIntUnsafe(fourBytes = bytes.take(4).toArray, order = ByteOrder.BIG_ENDIAN)) match {
//      case Success(n) => Right(bytes.drop(4), n)
//      case Failure(_) => Left("Could not convert bytes to Int")
//    }
//  })

  /**
   * TODO 5
   * Reads an Int from input using Little Endian order.
   *
   * Hint: Consider using the method replicateA in Applicative.
   */
  def getIntLE: Get[Int] = getByte.replicateA(4).map { bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.LITTLE_ENDIAN)
  }

  /**
   * TODO 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = getByte.replicateA(n).map { bytes =>
    new String(bytes.toArray)
  }
//  def getString(n: Int): Get[String] = Get(bytes => {
//    if (bytes.length < n) Left("Insufficient input")
//    else Try(new String(bytes.take(n).toArray)) match {
//      case Success(s) => Right(bytes.drop(n), s)
//      case Failure(_) => Left("Could not convert bytes to String")
//    }
//  })

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /**
   * TODO 7
   * Instance of monad error for Get.
   */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    // function that runs fa on bytes, then if successful, extracts output a and runs f(a) on remaining bytes
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get { bytes =>
      fa.run(bytes) match {
        case Right((remainingBytes, a)) => f(a).run(remainingBytes)
        case Left(e) => Left(e)
      }
    }

    // returns the input without touching the bytes
    override def pure[A](x: A): Get[A] = Get(bytes => Right(bytes, x))

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = Get { bytes =>
      Monad[({ type L[T] = Either[String, T] })#L].tailRecM((bytes, a)) { case (bytes, a) =>
        f(a).run(bytes).map { case (bytes, eab) =>
          eab match {
            case Right(b) => Right((bytes, b))
            case Left(a) => Left((bytes, a))
          }
        }
      }
    }

    override def raiseError[A](e: String): Get[A] = Get(_ => Left(e))

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] = Get { bytes =>
      fa.run(bytes) match {
        case r @ Right(_) => r
        case Left(e) => f(e).run(bytes)
      }
    }
  }

  /**
   * TODO 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   *
   * Hint: One possible way of doing this is to use scalacheck to build
   * a generator of List[Byte], then sample it several times (e.g. 32)
   * and check that running both Gets yields the same result every time.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = Eq.instance { (get1, get2) => {
    val nSamples = 32
    val genBytes = Gen.listOf(Gen.choose(0, 255).map(_.toByte))
    Iterator
      .continually(genBytes.sample) // this is an Iterator[Option[List[Byte]]]
      .take(nSamples) // still Iterator[Option[List[Byte]]]
      .flatten // turns into Iterator[List[Byte]]
      .forall(bytes => get1.run(bytes) === get2.run(bytes))
  }}

  /**
   * TODO 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    /**
     * Read the docs for combine and come up with an instance that does not
     * alter the behaviour of any Get it is combined with.
     *
     * Think about what should happen to the input bytes, and what would be a
     * suitable result.
     */
    override def empty: Get[A] = Monad[Get].pure(Monoid[A].empty)
      // Get(bytes => Right(bytes, Monoid[A].empty))

    /**
     * Combining two Get[A] instances should yield a new Get[A] instance which
     * runs both Gets in sequence and yields the combined result.
     *
     * If any of the Gets fails, the combined Get should fail with that same error.
     *
     * Check the tests for details.
     */
      // mapN does x and y sequentially already?
    override def combine(x: Get[A], y: Get[A]): Get[A] = (x, y).mapN((a1, a2) => a1 |+| a2)
//      Get { bytes =>
//      x.run(bytes) match {
//        case Right((remainingBytes1, xRes)) =>
//          y.run(remainingBytes1) match {
//            case Right((remainingBytes2, yRes)) => Right((remainingBytes2, Monoid[A].combine(xRes, yRes)))
//            case Left(e2) => Left(e2)
//          }
//        case Left(e1) => Left(e1)
//      }
//    }
  }
}