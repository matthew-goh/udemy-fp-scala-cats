import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

// We could also define our instances of ByteEncoder as vals
// By defining the instance method, we capture the boilerplate that this involves
object ByteEncoder {
//  implicit val stringByteEncoder: ByteEncoder[String] = new ByteEncoder[String] {
//    override def encode(s: String): Array[Byte] = s.getBytes()
//  }

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A): Array[Byte] = f(a)
  }

  implicit val stringByteEncoder: ByteEncoder[String] = instance(_.getBytes()) // instance[String](s => s.getBytes())
}

implicit val rot3StringByteEncoder: ByteEncoder[String] =
  ByteEncoder.instance(_.getBytes().map(b => (b+3).toByte))
