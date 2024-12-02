import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] = {
    a.getBytes()
  }
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

//implicit object OptionStringByteEncoder extends ByteEncoder[Option[String]] {
//  override def encode(a: Option[String]): Array[Byte] = {
//    a match {
//      case Some(s) => StringByteEncoder.encode(s)
//      case None => Array[Byte]()
//    }
//  }
//}

ByteEncoder[String].encode("hello")
ByteEncoder[Int].encode(1000)

// We were very specific about encoding Option[String]
// But what if we want to encode any Option? The code would be very similar

// given a ByteEncoder for A, obtain one for Option[A]
implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
  override def encode(a: Option[A]): Array[Byte] = {
    a match {
      case Some(value) => encA.encode(value)
      case None => Array[Byte]()
    }
  }
}

// compiler knows to build an instance of e.g. ByteEncoder[Option[String]] using the implicit def optionEncoder()
ByteEncoder[Option[String]].encode(Option("world"))
ByteEncoder[Option[Int]].encode(Option(1000))
