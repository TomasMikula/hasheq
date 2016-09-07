package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

/** Hash function for elements of type `A`.
  *
  * @tparam A
  * @tparam Eq Equivalence relation on `A` that this hash function is compatible with.
  *            Every hash function is compatible with equality, by definition of equality.
  *            For a more coarse equivalence relation, one has to make sure the hash function
  *            does not distinguish between equivalent elements.
  */
trait Hash[A, Eq <: Equiv[A]] {
  def hash(a: A): Int
}

object Hash {
  def apply[A, Eq <: Equiv[A]](implicit ev: Hash[A, Eq]): Hash[A, Eq] = ev

  trait Laws extends Equiv.Laws {
    def hashConsistency[A, Eq <: Equiv[A]](a: A, b: A)(implicit A: Hash[A, Eq], E: Eq): Boolean =
      if(E.equiv(a, b)) hash(a) == hash(b)
      else true

    protected def hash[A, Eq <: Equiv[A]](a: A)(implicit A: Hash[A, Eq]): Int = A.hash(a)
  }

  def properties[A, Eq <: Equiv[A]](name: String = "HashEq", laws: Laws = new Laws {})(implicit H: Hash[A, Eq], E: Eq, A: Arbitrary[A]): Properties = {
    val p = Equiv.properties(name, laws)
    p.property("hashConsistency") = forAll(laws.hashConsistency[A, Eq] _)
    p
  }
}
