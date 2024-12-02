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
}

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

// use an implicit parameter for the instance of the type class so it doesn't have to be called explicitly
object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("Users/matthew.goh/Documents/Training/udemy-fp-scala-cats/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes().map(b => (b+3).toByte)
}

FileChannel.write("hello")

// Common situation:
// For each type A, we have
// - one main instance (i.e. used most of the time) of ByteEncoder
// - a couple of other instances for specific use cases

// How can we infer the instance automatically?
// Goal 1: use the main instance by default
// - Make enc implicit in the write method
// - Move the instances of ByteEncoder into a companion object and mark them as implicit
// Goal 2: provide a different instance for specific use cases
// - EITHER pass it in explicitly, e.g. FileChannel.write("hello")(Rot3StringByteEncoder)
// - OR mark it as implicit outside the companion object - the compiler decides which instance to use; the one in direct scope takes precedence

// What if we have custom types?
// Of course, we could put SwtichByteEncoder into the companion object of ByteEncoder
// But what if ByteEncoder is imported and we can't edit it?
// For the default instance: Define a companion object for the *custom type* and put SwtichByteEncoder there!
// Again, can then define variants for specific use cases directly in scope
case class Switch(isOn: Boolean)
object Switch {
  implicit object SwtichByteEncoder extends ByteEncoder[Switch] {
    // return an array of 1 byte: '1' if isOn is true, '0' otherwise (byte for the character 1 or 0)
    override def encode(s: Switch): Array[Byte] =
      Array(if(s.isOn) '1'.toByte else '0'.toByte)
  }
}

FileChannel.write(Switch(false))
