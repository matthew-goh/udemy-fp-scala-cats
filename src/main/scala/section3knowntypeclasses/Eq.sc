import cats._
import cats.implicits._
import cats.kernel.laws.{IsEq, IsEqArrow}

// cats provides an Eq type class with apply, instance, and various implementations

// How can we use this to check if 2 accounts are equal?
case class Account(id: Long, number: String, balance: Double, owner: String)

// companion object contains implementations of Eq for Account
object Account {
  // fromUniversalEquals is from cats: builds an operator that compares values with == from Scala
  implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals

  // other instances for specific cases - called explicitly when needed
  object Instances {
    // use Eq's implementation for Long to compare by id, get it from implicit scope
    implicit def byIdEq(implicit eqLong: Eq[Long]): Eq[Account] =
      Eq.instance[Account]((a1, a2) => eqLong.eqv(a1.id, a2.id))
    // use by() to transform accounts into values that can be compared using the implicit parameter
    implicit def byIdEq2(implicit eqLong: Eq[Long]): Eq[Account] = Eq.by(_.id)

    // compare by number
    implicit def byNumber(implicit eqStr: Eq[String]): Eq[Account] = Eq.by(_.number)
  }
}

val account1 = Account(1, "123-56", 1000, "Leandro")
val account2 = Account(2, "123-56", 1500, "Martin")
// use apply method for this call - universalEq by default
Eq[Account].eqv(account1, account2) // false
// using other instances
Account.Instances.byIdEq.eqv(account1, account2)
Account.Instances.byNumber.eqv(account1, account2)

// cats provides syntax via === to use Eq
//import Account.Instances.byNumber // if this is imported, it is in direct scope and takes precedence over universalEq
implicit val eqToUse: Eq[Account] = Account.Instances.byNumber // this does the same
account1 === account2


//trait Eq[A] {
//  def eqv(fst: A, snd: A): Boolean
//}
//
//// Laws
//trait EqLaws[A] {
//  // convention is one uppercase letter
//  def E: Eq[A]
//
//  // IsEq defers the comparison until the test is run; then use <-> with it
//  def reflexivity(x: A): IsEq[A] = x <-> x // x = x
//
//  def symmetry(x: A, y: A): IsEq[Boolean] = E.eqv(x, y) <-> E.eqv(y, x) // x = y iff y = x
//
//  def antiSymmetryEq(x: A, y: A, f: A => A): IsEq[Boolean] =
//    (!E.eqv(x, y) || E.eqv(f(x), f(y))) <-> true // if x = y, then f(x) = f(y)
//
//  def transitivity(x: A, y: A, z: A): IsEq[Boolean] =
//    (!(E.eqv(x, y) && E.eqv(y, z)) || E.eqv(x, z)) <-> true // if x = y and y = z, then x = z
//}
