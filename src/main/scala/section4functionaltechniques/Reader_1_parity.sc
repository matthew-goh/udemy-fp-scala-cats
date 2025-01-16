import cats._
import cats.implicits._
import cats.data._

// Think of Reader as a wrapper over a function, e.g. this wraps an Int => String
val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")
signReader.run(0) // zero

// even or odd
val parityReader: Reader[Int, String] = Reader(n => if(n % 2 == 0) "even" else "odd")
parityReader.run(1)

// combine readers - use flatMap or for comprehension
val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader // sign is the output of signReader.run()
    parity <- parityReader
  } yield s"$sign and $parity"
descriptionReader.run(-2)

// without using the apply method
val addOneReader: Reader[Int, Int] =
  for {
//    env <- Reader((x: Int) => x) // ask - a Reader[Int, Int] that reads the environment
    env <- Reader(identity[Int])
  } yield env + 1

// use the environment as the place for injecting dependencies
case class Person(id: Long, name: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service

  trait Service {
    def findAccountById(id: Long): Account
  }
}
trait LiveAccountRepository extends AccountRepository {
  override val accountRepository: Service = new Service {
    override def findAccountById(id: Long): Account = Account(id, 2)
  }
}

trait PersonRepository {
  val personRepository: Service

  trait Service {
    def findPersonById(id: Long): Person
  }
}
trait LivePersonRepository extends PersonRepository {
  override val personRepository: Service = new Service {
    override def findPersonById(id: Long): Person = Person(id, "Leandro")
  }
}

// add 1 to the id and return that account
// how can we call findAccountById if there's no AccountRepository in the function?
def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountModule <- Reader(identity[AccountRepository]) // read the environment
    account = accountModule.accountRepository.findAccountById(id + 1) // use = since this is not part of the flatMap chain
  } yield account

// need both repositories - how do we use 2 dependencies?
def findOwnerNameByAccountId(id: Long): Reader[PersonRepository with AccountRepository, String] =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    owner = personModule.personRepository.findPersonById(account.ownerId)
  } yield owner.name

// type alias for the multiple dependencies
type Env = PersonRepository with AccountRepository
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository
//val testEnv: Env = new FakePersonRepository with FakeAccountRepository
findOwnerNameByAccountId(1).run(liveEnv) // run() requires a PersonRepository with AccountRepository

// what if both dependencies have a method with the same name?
trait A {
  def f(): String = "A"
}
trait B {
  def f(): String = "B"
}

val c = new A with B
//c.f() // error due to conflict

// Solution: define an inner trait Service containing the method implementations
// and assign the Service to a val with a unique name