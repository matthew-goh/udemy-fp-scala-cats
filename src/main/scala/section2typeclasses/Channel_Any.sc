import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

// Pros:
//-Simple interface - one write method and parameter
// Cons:
//-Unhandled type causes exception
//-Method has 2 responsibilities (getting the bytes, then writing)

trait Channel {
  def write(obj: Any): Unit
} 

object FileChannel extends Channel {
  override def write(obj: Any): Unit = {
    // need different implementations for different input types
    val bytes: Array[Byte] = obj match {
      case n: Int => {
        val bb = ByteBuffer.allocate(4) // allocate 4 bytes
        bb.putInt(n)
        bb.array()
      }
      case s: String => s.getBytes() // s.map(c => c.toByte).toArray
      case invalid => throw new Exception("unhandled")
    }

    // class that lets us use the output stream - handle closing even if there are exceptions etc
    Using(new FileOutputStream("Users/matthew.goh/Documents/Training/udemy-fp-scala-cats/test.txt")) { os =>
      os.write(bytes)
      os.flush()
      // println("File written")
    }
  }
}

FileChannel.write("hello")
//println(java.lang.System.getProperty("user.dir"))
