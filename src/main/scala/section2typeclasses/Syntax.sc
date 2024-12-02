import java.nio.ByteBuffer
import scala.util.Try

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] = a.getBytes
}

// EXTENSION METHOD
// use an implicit class to call encode as if the type A has it already
// - make it a value class to prevent performance degradation from creating a new instance every time .encode is called
implicit class ByteEncoderOps[A](val a: A) extends AnyVal {
  def encode(implicit enc: ByteEncoder[A]): Array[Byte] =
    enc.encode(a)
}

5.encode // -> new ByteEncoderOps[Int](5).encode
"hello world".encode


trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

implicit object IntByteDecoder extends ByteDecoder[Int] {
  override def decode(bytes: Array[Byte]): Option[Int] = {
    if(bytes.length != 4) None
    else {
      val bb = ByteBuffer.allocate(4)
      bb.put(bytes)
      bb.flip()
      Some(bb.getInt())
    }
  }
}

//implicit object StringByteDecoder extends ByteDecoder[String] {
//  override def decode(bytes: Array[Byte]): Option[String] =
//    Try(new String(bytes)).toOption
//}

implicit class ByteDecoderOps[A](bytes: Array[Byte]) {
  def decode(implicit dec: ByteDecoder[A]): Option[A] =
    dec.decode(bytes)
}

// works when there is only one ByteDecoder; otherwise don't know which to use given bytes alone
Array[Byte](0, 0, 0, 5).decode
Array[Byte](0, 0, 0, 0, 5).decode
Array[Byte](104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100).decode
