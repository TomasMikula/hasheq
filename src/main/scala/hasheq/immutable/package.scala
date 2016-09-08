package hasheq

import org.scalacheck.{Arbitrary, Properties}

import scala.language.higherKinds

package object immutable {

  type HashSet[A] = HashSetoid[A, Equal[A]]

  object HashSet {
    def empty[A]: HashSet[A] = HashSetoid.empty[A, Equal[A]]
    def apply[A](elems: A*)(implicit A: Hash[A, Equal[A]], E: Equal[A]): HashSet[A] = HashSetoid(elems:_*)

    def setoidInstance[A](implicit A: HashEqual[A]): Setoid[HashSetoid, A, Equal[A]] = HashSetoid.setoidInstance[A, Equal[A]]
  }

  object SetRepr {
    def properties[S[_], A]: PropBuilder[S, A] = PropBuilder()

    final case class PropBuilder[S[_], A]() {
      def apply[S0[A0, _ <: Equiv[A0]]](name: String = "SetRepr")(implicit ev: S[A] =:= S0[A, Equal[A]], S: Setoid[S0, A, Equal[A]], EA: Equal[A], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
        Setoid.properties[S0, A, Equal[A]](name)
    }
  }
}
