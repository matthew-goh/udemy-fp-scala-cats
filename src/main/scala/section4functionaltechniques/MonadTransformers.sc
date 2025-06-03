import cats._
import cats.implicits._
import cats.data._

case class Account(id: Long, balance: Double) {
  def updateBalance(f: Double => Double): Account =
    copy(balance = f(balance))
}

trait AccountRepo {}
val dummyRepo = new AccountRepo {}

type ErrorOr[A] = Either[String, A]
type AccountOpOld[A] = Reader[AccountRepo, ErrorOr[A]]

// Monad Transformer: redefine AccountOp[A]
// ReaderT[ monad to compose, dependency, success value ]
// When run with an AccountRepo, AccountOp transforms it into an Either[String, A]
// Note: Kleisli is a type alias for ReaderT
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]

// Enriching signatures: error handing, dependency injection
// This method returns an Account, can fail with a string error and needs an AccountRepo as a dependency
def openAccount(accountNumber: String, ownerSsn: String): Reader[AccountRepo, Either[String, Account]] = ???

def findAccountById(id: Long): AccountOp[Account] = ???
def saveAccount(account: Account): AccountOp[Account] = ???

// this is one solution using the old AccountOp, but not optimal due to nesting
//def depositMoney(accountId: Long, amount: Double): AccountOp[Account] =
//  findAccountById(accountId).flatMap{
//    case Right(account) => saveAccount(account.updateBalance(_ + amount))
//    case e @ Left(_) => Reader(_ => e)
//  }

// Monad Transformer: redefine AccountOp[A]
def depositMoney(accountId: Long, amount: Double): AccountOp[Account] = for {
  account <- findAccountById(accountId)
  savedAccount <- saveAccount(account.updateBalance(_ + amount))
} yield savedAccount


// Suppose we have ErrorOr[B] and want to create ReaderT[ErrorOr, A, B]
ReaderT.liftF[ErrorOr, AccountRepo, Int](5.asRight[String]).run(dummyRepo) // need to provide ErrorOr[Int] to the liftF
ReaderT.liftF[ErrorOr, AccountRepo, Int]("hello".asLeft[Int]).run(dummyRepo)

// B -> ReaderT[ErrorOr, A, B]
5.pure[AccountOp].run(dummyRepo)

// ReaderT[ErrorOr, A, B] -> ReaderT[Option, A, B]
5.pure[AccountOp].mapF{
  case Right(x) => Some(x)
  case Left(_) => None
}.run(dummyRepo)

// apply method - gives a Kleisli
ReaderT((_: AccountRepo) => 5.asRight[String]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).run(dummyRepo)

5.pure[AccountOp].map(_ + 1).run(dummyRepo) // Right(6)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).map(_ + 1).run(dummyRepo) // Left(hello)

5.pure[AccountOp].flatMap(n => (n+1).pure[AccountOp]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).flatMap(n => (n+1).pure[AccountOp]).run(dummyRepo)


/////////////////
type ErrorOrOpt[A] = OptionT[ErrorOr, A] // a wrapper around ErrorOr[Option[A]] = Error[String, Option[A]]
type AccountOp2[A] = ReaderT[ErrorOrOpt, AccountRepo, A]

5.pure[AccountOp2].flatMap(n => (n+1).pure[AccountOp2]).run(dummyRepo).value // Right(Some(6))
// .value takes the Either[Option[A]] out of the OptionT wrapper

5.pure[ErrorOrOpt].flatMap(n => (n+1).pure[ErrorOrOpt]) // OptionT(Right(Some(6)))
OptionT(Option(5).asRight[String]).flatMap(n => (n+1).pure[ErrorOrOpt]) // same as above
OptionT(Option.empty[Int].asRight[String]).flatMap(n => (n+1).pure[ErrorOrOpt]) // OptionT(Right(None))
OptionT("boom".asLeft[Option[Int]]).flatMap(n => (n+1).pure[ErrorOrOpt]) // OptionT(Left(boom))

// subflatMap - operate on the Option alone rather than the whole ErrorOr
OptionT(Option(5).asRight[String]).subflatMap(n => Option(n+1)) // OptionT(Right(Some(6)))
OptionT(Option.empty[Int].asRight[String]).subflatMap(n => Option(n+1)) // OptionT(Right(None))
OptionT("boom".asLeft[Option[Int]]).subflatMap(n => Option(n+1)) // OptionT(Left(boom))

// semiflatMap - operate on the Either
OptionT(Option(5).asRight[String]).semiflatMap(n => Right(n+1)) // OptionT(Right(Some(6)))
OptionT(Option.empty[Int].asRight[String]).semiflatMap(n => Right(n+1)) // OptionT(Right(None))
OptionT("boom".asLeft[Option[Int]]).semiflatMap(n => Right(n+1)) // OptionT(Left(boom))

// flatMapF - return type is ErrorOr[Option[A]] instead of ErrorOrOpt[A] = OptionT[ErrorOr, A]
OptionT(Option(5).asRight[String]).flatMapF(n => Right(Some(n+1))) // OptionT(Right(Some(6)))


// The idea of a Monad transformer is that it allows you to compose monads and work with the value inside
// without having to worry about the intermediate types.
// Possible performance issues due to wrapping and unwrapping all the time
