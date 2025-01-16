import cats._
import cats.data._
import cats.implicits._

case class Person(name: String, age: Int)

type IsValid[A] = Validated[List[String], A]
def validateAge(age: Int): IsValid[Int] = ???
def validateName(name: String): IsValid[String] = ???
def validatePerson(person: Person): IsValid[Person] = {
  (validateName(person.name), validateAge(person.age)).mapN{
    (n, a) => Person(n, a) // (IsValid[name], IsValid[age]) => IsValid[Person]
  }
}

// type parameter for .valid is the error type
5.valid[String] // Valid(5)
// type parameter for .invalid is the valid type
"error".invalid[Int] // Invalid(error)

5.valid[List[String]] // Valid(5) - List is better for accumulating errors
// NonEmptyList is a cats class - always contains at least one element
5.valid[NonEmptyList[String]]
5.validNel[String] // same as 5.valid[NonEmptyList[String]]
"error".invalidNel[Int] // Invalid(NonEmptyList(error))

// but list concatenation is a bit slow - must iterate through first list
def concat[A](as: List[A], as2: List[A]): List[A] =
  as.foldRight(as2)((a, b) => a :: b)

concat(List(1,2,3), List(4,5,6))

// faster to use a non-empty chain
5.validNec[String]
"error".invalidNec[Int]

// ensure checks that some valid value passes some predicate
5.validNec[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)
// Invalid(Chain(number is not even))
// value unchanged if predicate is true

// if test is true, return Valid(5), else return Invalid(error)
Validated.cond(test = true, 5, "error")
Validated.condNec(test = false, 5, "error") // Invalid(Chain(error))

5.validNec[String].getOrElse(10) // unwraps the value if Valid
"error".invalidNec[Int].getOrElse(10)

// return another Validated if this one is an error
5.validNec[String].orElse(10.validNec[String])
"error".invalidNec[Int].orElse(10.validNec[String])

// converting to and from Either
5.validNec[String].toEither // Either[cats.data.NonEmptyChain[String],Int] = Right(5)
"error".invalidNec[Int].toEither // ither[cats.data.NonEmptyChain[String],Int] = Left(Chain(error))

Validated.fromEither[NonEmptyChain[String], Int](Right(5))
Validated.fromEither[NonEmptyChain[String], Int](Left(NonEmptyChain("error")))
