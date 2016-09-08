package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

/** Defines ''equivalence'' on type `A`, i.e. a binary relation that is
  * reflexive, symmetric and transitive.
  */
trait Equiv[A] {
  def equiv(a: A, b: A): Boolean
}

object Equiv {
  def apply[A](implicit ev: Equiv[A]): Equiv[A] = ev

  trait Laws {
    def reflexivity[A: Equiv](a: A): Boolean = equiv(a, a)
    def symmetry[A: Equiv](a: A, b: A): Boolean = equiv(a, b) == equiv(b, a)
    def transitivity[A: Equiv](a: A, b: A, c: A): Boolean =
      if(equiv(a, b) && equiv(b, c)) equiv(a, c)
      else true

    protected def equiv[A: Equiv](a: A, b: A): Boolean = Equiv[A].equiv(a, b)
  }

  def properties[A: Arbitrary](e: Equiv[A], name: String = "Equiv", laws: Laws = new Laws {}): Properties =
    new Properties(name) {
      implicit val ee: Equiv[A] = e
      property("reflexivity")  = forAll(laws.reflexivity[A] _)
      property("symmetry")     = forAll(laws.symmetry[A] _)
      property("transitivity") = forAll(laws.transitivity[A] _)
    }
}
