import cats._
import cats.implicits._
import cats.data._

val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")
signReader.run(0)

val parityReader: Reader[Int, String] = Reader(n => if(n % 2 == 0) "even" else "odd")
parityReader.run(1)

val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader
    parity <- parityReader
  } yield s"$sign and $parity"
descriptionReader.run(-2)

val addOneReader: Reader[Int, Int] =
  for {
    env <- Reader(identity[Int])
  } yield env + 1

case class Person(id: Long, name: String, emailAddress: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service

  trait Service {
    def findAccountById(id: Long): Account
    def saveAccount(account: Account): Unit
  }
}

trait LiveAccountRepository extends AccountRepository {
  override val accountRepository: Service = new Service {
    override def findAccountById(id: Long): Account = Account(id, 2)
    override def saveAccount(account: Account): Unit = println("Account saved")
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
    override def findPersonById(id: Long): Person = Person(2, "leandro", "leandro@gmail.com")
  }
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

def findOwnerNameByAccountId(id: Long): Reader[PersonRepository with AccountRepository, String] =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    owner = personModule.personRepository.findPersonById(account.ownerId)
  } yield owner.name

// 1. Define an EmailService module with a method sendEmail(address: String, text: String): Unit
trait EmailService {
  val emailService: Service

  trait Service {
    def sendEmail(address: String, text: String): Unit
  }
}

// 2. Define a LiveEmailService trait with a dummy implementation for the module
trait LiveEmailService extends EmailService {
  override val emailService: Service = new Service {
    override def sendEmail(address: String, text: String): Unit = println(s"Sent email to $address with content: $text")
  }
}

// 3. Update Env and liveEnv to include this new module
type Env = PersonRepository with AccountRepository with EmailService
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository with LiveEmailService
findOwnerNameByAccountId(1).run(liveEnv)

// 4. Add an emailAddress field to the Person class and change the PersonRepository implementation accordingly
// 5. Add a saveAccount(account: Account): Unit method to the AccountRepository module
// 6. Add a dummy implementation for saveAccount in LiveAccountRepository

// 7. Implement an openAccount(accountId, ownerId) method that will create and save a new account,
// and will notify the user via email. Use the reader monad, and the dependencies that you see fit.
def openAccount(accountId: Long, ownerId: Long): Reader[Env, Account] = // could also return Unit
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    emailModule <- Reader(identity[EmailService])
    newAccount = Account(accountId, ownerId)
    _ = accountModule.accountRepository.saveAccount(newAccount)
    owner = personModule.personRepository.findPersonById(ownerId)
    _ = emailModule.emailService.sendEmail(owner.emailAddress, "Account created successfully")
  } yield newAccount

// 8. Run the function using the live environment
openAccount(accountId = 1, ownerId = 1).run(liveEnv)
