package section2typeclasses

import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.ByteBuffer
import scala.util.Try

object Tests {
  trait ByteDecoder[A]{
    def decode(bytes:Array[Byte]): Option[A]
  }

  trait ByteEncoder[A] {
    def encode(o:A): Array[Byte]
  }

  trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

  // Laws are placed in a trait that contains in instance of the type class (ByteCodec)
  // Need one method for each law
  trait ByteCodecLaws[A]{
    def codec: ByteCodec[A]

    // Property: Encoding and then decoding a value should leave the original value unchanged
    def isomorphism(o: A): Boolean =
      codec.decode(codec.encode(o)) == Some(o)
  }

  // To test laws with discipline and ScalaCheck:
  // extend Laws provided by discipline
  trait ByteCodecTests[A] extends Laws {
    def laws: ByteCodecLaws[A] // override default

    // ruleset, forAll provided by ScalaCheck
    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
      "byteCodec",
      parent = None,
      props = "isomorphism" -> forAll(laws.isomorphism _)
    )
  }
  // companion object - simplify creation of ByteCodecTests
  object ByteCodecTests {
    def apply[A](implicit bc: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
      override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
        override def codec: ByteCodec[A] = bc
      }
    }
  }

  // define the encode and decode methods for Int, now extending ByteCodec
  // implicit enables it to be detected by ByteCodecTests .apply()
  implicit object IntByteCodec extends ByteCodec[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] = {
      if(bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip() // .put() has the ByteBuffer in write mode, must flip to read mode
        Some(bb.getInt)
      }
    }

    override def encode(o: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(o)
      bb.array()
    }
  }

//  // write the laws - provide the specific codec
//  object IntByteCodecLaws extends ByteCodecLaws[Int] {
//    override def codec: ByteCodec[Int] = IntByteCodec
//  }
//
//  // write the tests - provide the specific laws
//  object IntByteCodecTests extends ByteCodecTests[Int] {
//    override def laws: ByteCodecLaws[Int] = IntByteCodecLaws
//  }

  implicit object StringByteCodec extends ByteCodec[String] {
    override def decode(bytes: Array[Byte]): Option[String] =
      Try(new String(bytes)).toOption

    override def encode(s: String): Array[Byte] = s.getBytes()
  }

//  object StringByteCodecLaws extends ByteCodecLaws[String] {
//    override def codec: ByteCodec[String] = StringByteCodec
//  }
//
//  object StringByteCodecTests extends ByteCodecTests[String] {
//    override def laws: ByteCodecLaws[String] = StringByteCodecLaws
//  }
}

// ACTUAL TEST FRAMEWORK
import Tests._

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  // 2 params: name of test, ruleset from test object
  checkAll("ByteCodec[Int]", ByteCodecTests[Int].byteCodec)
  checkAll("ByteCodec[String]", ByteCodecTests[String].byteCodec)
}
