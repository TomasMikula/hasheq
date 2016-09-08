package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

/** Hash function for elements of type `A` that respects the equivalence identified by `Eq`.
  *
  * @tparam A
  * @tparam Eq Identifier of the equivalence relation on `A` that this hash function is compatible with.
  *            Every hash function is compatible with [[Equality]], by definition of equality.
  *            For a more coarse equivalence relation, one has to make sure the hash function
  *            does not distinguish between equivalent elements.
  */
trait HashEq[A, Eq] {
  def hash(a: A): Int
}

object HashEq {
  def apply[A, Eq](implicit ev: HashEq[A, Eq]): HashEq[A, Eq] = ev

  def apply[A, Eq](f: A => Int): HashEq[A, Eq] = new HashEq[A, Eq] {
    def hash(a: A): Int = f(a)
  }

  trait Laws extends Equiv.Laws {
    def hashConsistency[A, Eq](a: A, b: A)(implicit A: HashEq[A, Eq], E: Equiv[A, Eq]): Boolean =
      if(E.equiv(a, b)) hash(a) == hash(b)
      else true

    protected def hash[A, Eq](a: A)(implicit A: HashEq[A, Eq]): Int = A.hash(a)
  }

  def properties[A]: PropBuilder[A] = PropBuilder[A]()

  final case class PropBuilder[A]() {
    def apply[Eq](name: String = "Hash", laws: Laws = new Laws {}) (implicit H: HashEq[A, Eq], E: Equiv[A, Eq], A: Arbitrary[A]): Properties = {
      val p = Equiv.properties[A](name, laws)
      p.property("equivalence-compatibility") = forAll(laws.hashConsistency[A, Eq] _)
      p
    }
  }
}
