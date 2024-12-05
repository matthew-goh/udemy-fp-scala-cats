import MOption._
import cats._
import cats.implicits._

// Monad is the most important type class in the whole of functional programming
// trait Monad[F[_]] extends ... {
// methods are pure (from Applicative), flatMap (from FlatMap) and tailRecM (for stack safety, also from FlatMap)
// }

// Laws:
// monadLeftIdentity: F.pure(a).flatMap(f) <-> f(a) where f: A => F[B]
// monadRightIdentity: fa.flatMap(F.pure) <-> fa
// flatMapAssociativity: let f: A => F[B] and g: B => F[C],
// then fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))

// our own version of Option
sealed trait MOption[+A]

object MOption {
  case class MSome[+A](a: A) extends MOption[A]
  case object MNone extends MOption[Nothing]

  implicit val monadMOption: Monad[MOption] = new Monad[MOption] {
    override def pure[A](x: A): MOption[A] =
      MSome(x)

    override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] =
      fa match {
        case MSome(a) => f(a)
        case MNone => MNone
      }

    override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] = ???

    // implement map using pure and flatMap
    override def map[A, B](fa: MOption[A])(f: A => B): MOption[B] = flatMap(fa)(a => pure(f(a)))
  }
}


case class Person(name: String)
case class Account(balance: Double, owner: Person)
case class Transfer(source: Account, dest: Account, amount: Double)

def findPersonByName(name: String): MOption[Person]
def findAccountByPerson(person: Person): MOption[Account]
def findLastTransferBySourceAccount(account: Account): MOption[Transfer]
// implement this given that the above methods exist
def findLastTransferByPersonName(name: String): MOption[Transfer] = {
  for {
    person <- findPersonByName(name)
    acc <- findAccountByPerson(person)
    transfer <- findLastTransferBySourceAccount(acc)
  } yield transfer

  //  findPersonByName(name).flatMap { person =>
  //    findAccountByPerson(person).flatMap{ acc =>
  //      findLastTransferBySourceAccount(acc)
  //    }
  //  }

  //  findPersonByName(name) match {
  //    case MSome(person) => findAccountByPerson(person) match {
  //      case MSome(acc) => findLastTransferBySourceAccount(acc)
  //      case MNone => MNone
  //    }
  //    case MNone => MNone
  //  }
}
