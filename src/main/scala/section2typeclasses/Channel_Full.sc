import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.{Try, Using}

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

  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A): Array[Byte] = f(a)
  }
}


trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder {
  implicit object IntByteDecoder extends ByteDecoder[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] = {
      if(bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt)
      }
    }
  }

  implicit object StringByteDecoder extends ByteDecoder[String] {
    override def decode(bytes: Array[Byte]): Option[String] =
      Try(new String(bytes)).toOption
  }

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
  }
}

// Trait with both encode and decode
trait ByteCodec[A] extends ByteEncoder[A] with ByteDecoder[A]

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
  def read[A]()(implicit dec: ByteDecoder[A]): A
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("Users/matthew.goh/Documents/Training/udemy-fp-scala-cats/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }

  override def read[A]()(implicit dec: ByteDecoder[A]): A = ???
}

// Different instance for specific case
implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes().map(b => (b+3).toByte)
}


// Custom class
case class Switch(isOn: Boolean)
object Switch {
  implicit object SwtichByteEncoder extends ByteEncoder[Switch] {
    // return an array of 1 byte: '1' if isOn is true, '0' otherwise (byte for the character 1 or 0)
    override def encode(s: Switch): Array[Byte] =
      Array(if(s.isOn) '1'.toByte else '0'.toByte)
  }
}

FileChannel.write("hello")
FileChannel.write(Switch(false))
