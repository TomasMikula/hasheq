package hasheq

import org.scalacheck.{Arbitrary, Properties}

import scala.language.higherKinds

package object immutable {

  type HashSet[A] = HashSetoid[A, Equal[A]]

  object SetRepr {
    def properties[S[_], A]: PropBuilder[S, A] = PropBuilder()

    final case class PropBuilder[S[_], A]() {
      def apply[S0[A0, _ <: Equiv[A0]]](name: String = "SetRepr")(implicit ev: S[A] =:= S0[A, Equal[A]], S: Setoid[S0, A, Equal[A]], EA: Equal[A], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
        Setoid.properties[S0, A, Equal[A]](name)
    }
  }
}