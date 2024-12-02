import java.io.FileOutputStream
import scala.util.Using

// Pros:
//-unique responsibilities
//-easy to test
//-unhandled type (trying to write something that doesn't extend ByteEncodable) causes compile error
// Cons:
//-doesn't extend classes that we don't have control over, e.g. Int
//-can only provide one implementation - can only extend ByteEncodable once and provide one encode() method
//-overloaded interface - need extra method in the class extending ByteEncodable

trait ByteEncodable {
  def encode(): Array[Byte]
}

trait Channel {
  def write(obj: ByteEncodable): Unit
}

case class FullName(firstName: String, lastName: String) extends ByteEncodable {
  override def encode(): Array[Byte] = {
    firstName.getBytes ++ lastName.getBytes
  }
}

object FileChannel extends Channel {
  override def write(obj: ByteEncodable): Unit = {
    val bytes: Array[Byte] = obj.encode() // input obj must be ByteEncodable and have the encode() method

    Using(new FileOutputStream("Users/matthew.goh/Documents/Training/udemy-fp-scala-cats/test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}
