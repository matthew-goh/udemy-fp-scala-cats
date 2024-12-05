import cats._
import cats.implicits._

// cats has a Monoid type class:

//trait Monoid[A] extends Semigroup[A] {
//  def combine(x: A, y: A): A
//  def empty: A
//}

// empty acts as a neutral element for the combine operation (has no effect on the element it's combined with)
// Laws: associative, leftIdentity, rightIdentity
// combine(combine(x, y), z) = combine(x, combine(y, z)); combine(empty, x) = x; combine(x, empty) = x
// note: combine need not be commutative

case class Speed(metersPerSecond: Double) {
  def kilometersPerSec: Double = metersPerSecond / 1000.0
  def milesPerSec: Double = metersPerSecond / 1609.34
}

object Speed {
  def addSpeeds(s1: Speed, s2: Speed): Speed =
    Speed(s1.metersPerSecond + s2.metersPerSecond)

  implicit val eqSpeed: Eq[Speed] = Eq.fromUniversalEquals // needed for isEmpty
  implicit val monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds) // pass in empty value and combine function
}

Monoid[Speed].combine(Speed(1000), Speed(2000))
Monoid[Speed].empty
Monoid[Speed].combine(Speed(1000), Monoid[Speed].empty)
// syntax: |+| for combine
Speed(1000) |+| Speed(2000)
// combining > 2 elements
Monoid[Speed].combineAll(List(Speed(100), Speed(200), Speed(300)))
List(Speed(100), Speed(200), Speed(300)).combineAll

// isEmpty is from Eq
Monoid[Speed].isEmpty(Speed(100))
Monoid[Speed].isEmpty(Speed(0))


// Note: implicit keyword not used as there would be clashes (compiler can't resolve instance correctly)
val sumMonoid: Monoid[Int] = Monoid.instance(0, _+_)
val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)
def listMonoid[A]: Monoid[List[A]] = Monoid.instance(Nil, _ ++ _)
val stringMonoid: Monoid[String] = Monoid.instance("", _+_)

sumMonoid.combine(3, 4)
minMonoid.combine(minMonoid.empty, 667788999)
listMonoid[Boolean].combine(List(true, false), List(false, true))
stringMonoid.combine("hello ", "world")
