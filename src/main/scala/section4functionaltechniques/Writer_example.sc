import cats._
import cats.data._
import cats.implicits._

// Writer[W, A] calculates a value while we accumulate data in a log
// W is the type of the log (must have a semigroup instance so we can combine data)
// A is the computed value
// Writer has a Monad instance (hence Applicative)

object Tracked {
  // by fixing W, we can use Tracked as Monad, Applicative, etc
  type Tracked[A] = Writer[List[String], A]

  implicit def trackedShow[A: Show]: Show[Tracked[A]] = Show.show { ta =>
    val (log: List[String], a: A) = ta.run
    (log ++ List(a.show)).mkString("\n") // print each log on a separate line, followed by the value
  }
}
import Tracked._

case class Client(id: Long, name: String, age: Int)
object Client {
  def makeRaw(id: Long, name: String, age: Int): Tracked[Client] =
    Client(id, name, age).writer(List(s"Creating client $id"))
//  List(s"Creating client $id").tell.map(_ => Client(id, name, age))
}

case class Product(id: Long, name: String, unitPrice: Double)
object Product {
  def makeRaw(id: Long, name: String, unitPrice: Double): Tracked[Product] =
    Product(id, name, unitPrice).writer(List(s"Creating product $id"))
}

case class ShoppingCartItem(quantity: Int, product: Product) {
  def total: Double = quantity * product.unitPrice
}
object ShoppingCartItem {
  implicit val shoppingCartItemShow: Show[ShoppingCartItem] =
    Show.show(item => s"${item.quantity} x ${item.product.name}")

  def makeRaw(quantity: Int, productId: Long, productName: String, productUnitPrice: Double): Tracked[ShoppingCartItem] = {
    for {
      _ <- List(s"Creating shopping cart item for product id $productId").tell
      product <- Product.makeRaw(productId, productName, productUnitPrice)
    } yield ShoppingCartItem(quantity, product)

//    (List(s"Creating shopping cart item for product id $productId").tell,
//      Product.makeRaw(productId, productName, productUnitPrice)).mapN {(_, product) =>
//        ShoppingCartItem(quantity, product)
//    }
  }
}

case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {
  def total: Double = items.map(_.total).sum
}
object ShoppingCart {
  implicit val scShow: Show[ShoppingCart] = Show.fromToString

  def makeRaw(clientId: Long, clientName: String, clientAge: Int,
     items: List[(Int, Long, String, Double)]): Tracked[ShoppingCart] =
    for {
      _ <- List("Creating shopping cart").tell
      client <- Client.makeRaw(clientId, clientName, clientAge)
      shoppingCartItems <- items.traverse {
        case (quantity, productId, productName, productUnitPrice) =>
          ShoppingCartItem.makeRaw(quantity, productId, productName, productUnitPrice)
      } // Tracked[List[ShoppingCartItem]]
    } yield ShoppingCart(client, shoppingCartItems)
}

val sc = ShoppingCart.makeRaw(clientId = 123, clientName = "Leandro", clientAge = 70,
  items = List((1, 3, "eggs", 15), (4, 8, "milk", 30)))
Show[Tracked[ShoppingCart]].show(sc)


sealed trait Discount {
  val name: String
  // applies() returns true if this Discount applies to the specified client and item
  def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean
  def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double

  // get the amount by which a ShoppingCartItem is discounted
  def calculateDiscount(client: Client, shoppingCartItem: ShoppingCartItem): Tracked[Double] =
    if(applies(client, shoppingCartItem)) {
      // still calculate discount amount, then call writer()
      // which wraps the value in Writer with the specified log (W = List(...), A = discount amount)
      getDiscountedAmount(shoppingCartItem)
        .writer(List(s"Applied discount: $name"))
    } else {
      // same as Applicative[Tracked].pure(0d)
      0d.pure[Tracked]
    }
}

object Discount {
  object MoreThanFiveUnitsDiscount extends Discount {
    override val name = "10% discount on more than 5 units"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      shoppingCartItem.quantity > 5

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.1
  }

  object ElderlyDiscount extends Discount {
    override val name = "20% discount for people older than 65"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      client.age > 65

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.2
  }

  val allDiscounts: List[Discount] = List(MoreThanFiveUnitsDiscount, ElderlyDiscount)
}


// Requirement: Print details of applied discounts on screen
def calculateTotalDiscount(shoppingCart: ShoppingCart, discounts: List[Discount]): Tracked[Double] = {
//  (shoppingCart.items, discounts).mapN { (item, discount) =>
//    discount.calculateDiscount(shoppingCart.client, item) // gives a Tracked[Double] = Writer[List[String], Double]
//  }.combineAll
  // result of mapN is a List[Tracked[Double]], combine into a single Tracked[Double] using Monoid instances of Tracked and Double

  // Method 2:
  (shoppingCart.items, discounts)
    .tupled // get a single list of tuples
    .traverse { case (i, d) => d.calculateDiscount(shoppingCart.client, i) }
    .map(_.sum)
  // traverse goes over the List[(ShoppingCartItem, Discount)], converting each (ShoppingCartItem, Discount) to a Tracked[Double]
  // then effectively changes the List[Tracked[Double]] to a Tracked[List[Double]]

  // Method 3:
//  (shoppingCart.items, discounts)
//    .tupled // get a single list of tuples
//    .foldMap { case (i, d) => d.calculateDiscount(shoppingCart.client, i) }
  // foldMap does calculateDiscount for each tuple, then combines (sums) them using the Monoid instance of Double
}

// subtract the discount from the total, keeping it within a Tracked
def calculateTotal(shoppingCart: ShoppingCart): Tracked[Double] = {
  calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
    .map(a => shoppingCart.total - a)
}

val client = Client(1, "leandro", 70)
val milk = Product(1, "milk", 15.0)
val eggs = Product(1, "eggs", 25.0)
val items = List(
  ShoppingCartItem(15, milk), // 15 * 15 = 225 (30% is 77.5)
  ShoppingCartItem(30, eggs) // 30 * 25 = 750 (30% is 225)
)
val shoppingCart = ShoppingCart(client, items)
calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
Show[Tracked[Double]].show(calculateTotalDiscount(shoppingCart, Discount.allDiscounts)) // 77.5 + 225 = 292.5
Show[Tracked[Double]].show(calculateTotal(shoppingCart)) // 975 - 292.5 = 682.5
