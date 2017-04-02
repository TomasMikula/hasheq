package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

/** Defines ''equivalence'' on type `A`, i.e. a binary relation that is
  * reflexive, symmetric and transitive.
  *
  * @tparam Eq a "tag" that (uniquely) identifies the kind of equivalence.
  *            As there are multiple valid equivalence relations for any
  *            type with more than one inhabitant, this tag is used to
  *            distinguish among them.
  */
trait Equiv[A, Eq] {
  def equiv(a: A, b: A): Boolean
}

object Equiv {
  def apply[A, Eq](implicit ev: Equiv[A, Eq]): Equiv[A, Eq] = ev

  def instance[A, Eq](f: (A, A) => Boolean): Equiv[A, Eq] = new Equiv[A, Eq] {
    def equiv(a: A, b: A): Boolean = f(a, b)
  }

  trait Laws {
    def reflexivity[A, Eq](a: A)(implicit E: Equiv[A, Eq]): Boolean = equiv(a, a)
    def symmetry[A, Eq](a: A, b: A)(implicit E: Equiv[A, Eq]): Boolean = equiv(a, b) == equiv(b, a)
    def transitivity[A, Eq](a: A, b: A, c: A)(implicit E: Equiv[A, Eq]): Boolean =
      if(equiv(a, b) && equiv(b, c)) equiv(a, c)
      else true

    protected def equiv[A, Eq](a: A, b: A)(implicit E: Equiv[A, Eq]): Boolean = E.equiv(a, b)
  }

  def properties[A]: PropBuilder[A] = PropBuilder[A]()

  final case class PropBuilder[A]() {
    def apply[Eq](name: String = "Equiv", laws: Laws = new Laws {})(implicit E: Equiv[A, Eq], A: Arbitrary[A]): Properties =
      new Properties(name) {
        property("reflexivity")  = forAll(laws.reflexivity[A, Eq] _)
        property("symmetry")     = forAll(laws.symmetry[A, Eq] _)
        property("transitivity") = forAll(laws.transitivity[A, Eq] _)
      }
  }
}
