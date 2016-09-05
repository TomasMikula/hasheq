package hasheq

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

trait Eq[A] {
  def equal(a: A, b: A): Boolean
}

object Eq {
  def apply[A](implicit ev: Eq[A]): Eq[A] = ev

  trait Laws {
    def reflexivity[A: Eq](a: A): Boolean = equal(a, a)
    def symmetry[A: Eq](a: A, b: A): Boolean = equal(a, b) == equal(b, a)
    def transitivity[A: Eq](a: A, b: A, c: A): Boolean =
      if(equal(a, b) && equal(b, c)) equal(a, c)
      else true

    protected def equal[A: Eq](a: A, b: A): Boolean = Eq[A].equal(a, b)
  }

  def properties[A: Eq: Arbitrary](name: String = "Eq", laws: Laws = new Laws {}): Properties =
    new Properties(name) {
      property("reflexivity")  = forAll(laws.reflexivity[A] _)
      property("symmetry")     = forAll(laws.symmetry[A] _)
      property("transitivity") = forAll(laws.transitivity[A] _)
    }
}
