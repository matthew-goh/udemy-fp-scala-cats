import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

// Pros:
//-ByteEncoder can be instanced by any type
//-Cleaner interface - add methods for different types outside
//-Multiple implementations possible for same type (e.g. Big Endian and Little Endian for Int)

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait Channel {
  def write[A](obj: A, enc: ByteEncoder[A]): Unit
}

object FileChannel extends Channel {
  override def write[A](obj: A, enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("Users/matthew.goh/Documents/Training/udemy-fp-scala-cats/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

// To use this write method, we need to provide some encoders for specific input types
object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4) // allocate 4 bytes
    bb.putInt(n)
    bb.array()
  }
}

object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes()
}

//FileChannel.write(5, IntByteEncoder)
FileChannel.write("hello", StringByteEncoder)
