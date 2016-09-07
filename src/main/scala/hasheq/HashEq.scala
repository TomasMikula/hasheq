package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

trait HashEq[A] extends Equiv[A] {
  def hash(a: A): Int
}

object HashEq {
  def apply[A](implicit ev: HashEq[A]): HashEq[A] = ev

  trait Laws extends Equiv.Laws {
    def hashConsistency[A: HashEq](a: A, b: A): Boolean =
      if(equiv(a, b)) hash(a) == hash(b)
      else true

    protected def hash[A: HashEq](a: A): Int = HashEq[A].hash(a)
  }

  def properties[A: HashEq: Arbitrary](name: String = "HashEq", laws: Laws = new Laws {}): Properties = {
    val p = Equiv.properties(name, laws)
    p.property("hashConsistency") = forAll(laws.hashConsistency[A] _)
    p
  }
}
