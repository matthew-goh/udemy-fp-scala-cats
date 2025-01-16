import cats._
import cats.data._
import cats.implicits._

type Tracked[A] = Writer[List[String], A]

// value only:
8.pure[Tracked] // WriterT((List(),8))
// note: Writer is a type alias for the WriterT monad transformer

// log only:
List("hello").tell // Writer[List[String],Unit] = WriterT((List(hello),()))

// value and log:
val x = 10.writer(List("a ten")) // WriterT((List(a ten),10))
// explicitly use the apply() method
Writer(List("an eight"), 8)

// reset the log
x.reset
// add the log to the value (creates a tuple)
x.listen // WriterT((List(a ten), (10,List(a ten))))

// unwrap
x.value // 10
x.run // (List(a ten),10)

// Writer is a Functor, so has a map() method
// map() modifies the value but not the log
x.map(_ + 1) // WriterT((List(a ten),11))
// flatMap() modifies the value and combines the logs
x.flatMap(i => (i * 2).writer(List("multiplied by 2"))) // WriterT((List(a ten, multiplied by 2),20))

// mapN() operates on the values and combines the logs
val y = 15.writer(List("a fifteen"))
(x, y).mapN((a, b) => a + b) // WriterT((List(a ten, a fifteen),25))

// combining Writers (list concatenation, addition)
// List and Int are semigroups
x |+| y
// List is a Foldable - can use combineAll if the element type is a Monoid
val z = 20.writer(List("a twenty"))
List(x, y, z).combineAll
