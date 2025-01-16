import cats.data.Reader

case class Account(id: Long, balance: Double)

// DB Client
class DbClient {
  //def executeStatement(Statement): Boolean
  //def executeQuery[A: SqlDecoder](Query): A
}

// Account service

// v1: need to pass dbClient explicitly to every function call
//class AccountService {
//  def save(account: Account): DbClient => Boolean
//  def findById(id: Long): DbClient => Account
//  def delete(id: Long): DbClient => Boolean
//
//  def update(id: Long, upd: Account => Account): DbClient => Boolean =
//    dbClient => {
//      val account = findById(id)(dbClient)
//      val updatedAccount = upd(account)
//      save(updatedAccount)(dbClient)
//    }
//
//  def transferFunds(id1: Long, id2: Long, amount: Double): DbClient => Boolean
//}

// v2: use the Reader monad
class AccountService {
  def save(account: Account): Reader[DbClient, Boolean] = ???
  def findById(id: Long): Reader[DbClient, Account] = ???
  def delete(id: Long): Reader[DbClient, Boolean] = ???

  def update(id: Long, upd: Account => Account): Reader[DbClient, Boolean] =
    for {
      account <- findById(id)
      success <- save(upd(account))
    } yield success

  def transferFunds(id1: Long, id2: Long, amount: Double): Reader[DbClient, Boolean] = ???
}
