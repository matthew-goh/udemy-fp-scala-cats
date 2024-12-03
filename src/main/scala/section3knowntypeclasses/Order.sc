import cats._
import cats.implicits._

// cats provides an Order type class that represents a particular order for a type
// Order's from() method is analogous to instance()

// How can we order accounts? By number / balance / owner
case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  // DEFAULT: compare by id, use the order for long
  // implicit val orderById: Order[Account] = Order.from((a1, a2) => Order[Long].compare(a1.id, a2.id))
  implicit def orderById(implicit orderLong: Order[Long]): Order[Account] =
    Order.from((a1, a2) => orderLong.compare(a1.id, a2.id))

  object Instances {
    // by() transforms accounts into something that can be compared
    implicit val orderByNumber: Order[Account] = Order.by(_.number)
    // implicit def orderByNumber(implicit orderStr: Eq[String]): Order[Account] = Order.by(_.number)

    implicit def orderByBalance(implicit orderDouble: Order[Double]): Order[Account] = Order.by(_.balance)
  }
}

def sort[A](list: List[A])(implicit orderA: Order[A]): List[A] = {
  // Order provides an auxiliary method that converts an instance of it to an ordering that can be used by sorted()
  list.sorted(orderA.toOrdering)
}

val account1 = Account(1, "442-21", 3000, "Julia")
val account2 = Account(2, "442-21", 2500, "Romeo")
//val account3 = Account(3, "442-21", 3000, "Julia")
sort(List(account1, account2)) // compares by id by default

//import Account.Instances.orderByBalance
//sort(List(account1, account2))

// syntax
account1 compare account2 // 1, means account1 ">" account2 using orderByBalance
// would be -1 using orderById
account1 min account2
account1 max account2

// get reverse ordering from one already defined
implicit val orderByIdDesc: Order[Account] = Order.reverse(Account.orderById)
sort(List(account1, account2))


//trait Order[A] extends PartialOrder[A] {
//  // < 0 if fst "<" snd, = 0 if fst "=" snd, > 0 if fst ">" snd
//  def compare(fst: A, snd: A): Int
//}

// Laws
// reflexivityLt: x <= x
// antisymmetry: if x <= y and y <= x, then x = y
// transitivity: if x <= y and y <= z then x <= z
// totality: x <= y or x <= x
