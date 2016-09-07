package hasheq

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}

trait Equal[A] extends Equiv[A] {

  def equal(a1: A, a2: A): Boolean

  final override def equiv(a1: A, a2: A): Boolean = equal(a1, a2)

}

object Equal {
  def apply[A](implicit ev: Equal[A]): Equal[A] = ev

  trait Laws extends Equiv.Laws {
    def substitutability[A: Equal, B: Equiv](a: A, b: A, f: A => B): Boolean =
      if(equal(a, b)) equiv(f(a), f(b))
      else true

    protected def equal[A: Equal](a: A, b: A): Boolean = Equal[A].equal(a, b)
  }

  def properties[A](name: String = "Equal", laws: Laws = new Laws {})(implicit E: Equal[A], A: Arbitrary[A], AI: Arbitrary[A => Int]): Properties = {
    val p = Equiv.properties(name, laws)
    import hasheq.std.int._
    p.property("substitutability") = forAll(laws.substitutability[A, Int] _)
    p
  }
}