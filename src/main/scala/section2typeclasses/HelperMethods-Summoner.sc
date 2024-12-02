import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object IntByteEncoder extends ByteEncoder[Int] {
    override def encode(n: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4) // allocate 4 bytes
      bb.putInt(n)
      bb.array()
    }
  }

  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] = s.getBytes()
  }

  // ev for evidence
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes().map(b => (b+3).toByte)
}

// We can call ByteEncoder methods directly
ByteEncoder.StringByteEncoder.encode("hello")
// But this code is not very flexible
// - if we add Rot3StringByteEncoder outside the companion object, it isn't used

// How can we always use the current encoder in scope?
// Need a generic object in implicit scope
implicitly[ByteEncoder[String]].encode("hello")

// BUT we should provide a method in the type class that allows this without having to call 'implicitly'
// It finds an implicit instance in scope and returns it
// Call it apply, then we don't have to write the name of the method
ByteEncoder[String].encode("hello")
