import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}

package object hasheq {

  object Equality

  /** Equality relation on `A`. Equality is the finest equivalence,
    * satisfying an additional ''substitution property:''
    *  - ∀ a,b ∈ A, ∀ f ∈ (A -> B)
    *    - a =,,A,, b ⟹ f(a) =,,B,, f(b)
    */
  type Equal[A] = Equiv[A, Equality.type]

  object Equal {
    def apply[A](implicit ev: Equal[A]): Equal[A] = ev

    def apply[A](f: (A, A) => Boolean): Equal[A] = Equiv[A, Equality.type](f)

    trait Laws extends Equiv.Laws {
      def substitutability[A, B, EqB](a: A, b: A, f: A => B)(implicit EA: Equal[A], EB: Equiv[B, EqB]): Boolean =
        if(equal(a, b)) equiv(f(a), f(b))
        else true

      protected def equal[A: Equal](a: A, b: A): Boolean = Equal[A].equiv(a, b)
    }

    def properties[A](name: String = "Equal", laws: Laws = new Laws {})(implicit E: Equal[A], A: Arbitrary[A], AI: Arbitrary[A => Int]): Properties = {
      val p = Equiv.properties(name, laws)
      import hasheq.std.int._
      p.property("substitutability") = forAll(laws.substitutability[A, Int, Equality.type] _)
      p
    }
  }

  /** Hash function for elements of type `A` that isn't required to be
    * compatible with any particular equivalence relation on `A`. Note
    * that every (hash) function is compatible with equality (by the
    * definition of equality, due to the substitution property).
    * Equality is thus the only equivalence relation that this hash
    * function is assumed to be compatible with.
    */
  type Hash[A] = HashEq[A, Equality.type]
  object Hash {
    def apply[A](implicit ev: Hash[A]): Hash[A] = ev

    def apply[A](f: A => Int): Hash[A] = HashEq[A, Equality.type](f)
  }
}
