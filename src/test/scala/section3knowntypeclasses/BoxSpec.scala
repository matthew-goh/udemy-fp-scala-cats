package section3knowntypeclasses

import cats._
import cats.implicits._
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class BoxSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline with ScalaCheckDrivenPropertyChecks {
  implicit def eqBox[A](implicit eqA: Eq[A]): Eq[Box[A]] = Eq.by(_.value) // Not needed if you solved Box exercise

  // ScalaCheck has a concept of generator for random values
  // can use factory methods
  val genInt: Gen[Int] = Gen.choose(1, 10) // Int within this range (inclusive)
  val genInt2: Gen[Int] = Gen.oneOf(1, 5, 10)
  val genString: Gen[String] = Gen.alphaNumStr // alphanumeric string
  val genString2: Gen[String] = Gen.numStr // numeric string
  // generators can be combined since they have a flatMap method
  val genTuple: Gen[(Int, String)] =
    for {
      i <- genInt
      s <- genString
    } yield (i, s)

  // then wrap a generator in an Arbitrary
  val arbInt: Arbitrary[Int] = Arbitrary(genInt)

  // arbA.arbitrary is a Gen[A]
  // Arbitrary[A] -> Gen[A] -> Gen[Box[A]] -> Arbitrary[Box[A]]
  implicit def arbBoxA[A](implicit arbA: Arbitrary[A]): Arbitrary[Box[A]] =
    Arbitrary(arbA.arbitrary.map(Box.apply)) // inside is a Gen[Box[A]]

  // How to build an Arbitrary[A => A] (any function) given an Arbitrary[A] ?
  // Arbitrary[A] -> Gen[A] -> Gen[A => A] -> Arbitrary[A => A]
  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(arbA.arbitrary.map(a => (_: A) => a)) // use constant function that always returns whatever is inside arbA

  // test the laws of Eq: use the eqv ruleSet from EqTests
  // need to provide arbitraries: one for Box[Int], one for Box[Int] => Box[Int]
  checkAll("Eq[Box[Int]]", EqTests[Box[Int]].eqv)
  checkAll("Monad[Box]", MonadTests[Box].monad[Int, Int, Int])

  // what if we want to test our own properties?
  test("Wrapping and unwrapping yields original value") {
    // by default, this generates random ints using whichever Arbitrary[Int] is in scope
    // but we can specify the arbitrary using an implicit val or specify it explicitly, e.g. forall(genInt2, genString2)
//     implicit val myArb: Arbitrary[Int] = arbInt
    forAll { (i: Int, s: String) =>
      assert(Box(i).value eqv i)
      assert(Box(s).value eqv s)
      // note: some clashes arise when using ===, so use eqv instead
    }
  }
}