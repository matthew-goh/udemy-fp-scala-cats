import cats._
import cats.implicits._

// cats provides a Show type class for string representations
// Show has a show() method similar to instance()

// How can we show an account as a string? One way is Scala's default for case classes
case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  // fromToString is from cats: uses the usual toString from Scala
  implicit val toStringShow: Show[Account] = Show.fromToString

  object Instances {
    // "<owner> - $<balance>" (note: doesn't need an existing implicit instance of Show)
    implicit val byOwnerAndBalance: Show[Account] = Show.show{ account =>
      s"${account.owner} - $$${account.balance}" // note: escape the dollar sign
    }

    // "This account belongs to <owner>"
    implicit val prettyByOwner: Show[Account] = Show.show{ account =>
      s"This account belongs to ${account.owner}"
    }
  }
}

val leandro = Account(1, "123-45", 2000, "Leandro")
Show[Account].show(leandro) // using apply method, same as Account.toStringShow.show(leandro)
Account.Instances.byOwnerAndBalance.show(leandro)
Account.Instances.prettyByOwner.show(leandro)

// syntax
import Account.Instances.byOwnerAndBalance
leandro.show // uses whichever instance is in implicit scope


//trait Show[A] {
//  def show(a: A): String
//}
