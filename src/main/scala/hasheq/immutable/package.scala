package hasheq

import org.scalacheck.{Arbitrary, Properties}

import scala.language.higherKinds

package object immutable {

  type HashSet[A] = HashSetoid[A, Equality.type]
  object HashSet {
    def empty[A]: HashSet[A] = HashSetoid.empty[A, Equality.type]
    def apply[A](elems: A*)(implicit A: HashEq[A, Equality.type], E: Equal[A]): HashSet[A] = HashSetoid(elems:_*)

    def setoidInstance[A](implicit A: Hash[A]): Setoid[HashSetoid, A, Equality.type] = HashSetoid.setoidInstance[A, Equality.type]
  }

  type SetRepr[S[_, _], A] = Setoid[S, A, Equality.type]
  object SetRepr {
    def properties[S[_], A]: PropBuilder[S, A] = PropBuilder()

    final case class PropBuilder[S[_], A]() {
      def apply[S0[_, _]](name: String = "SetRepr")(implicit ev: S[A] =:= S0[A, Equality.type], S: SetRepr[S0, A], EA: Equal[A], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
        Setoid.properties[S0, A, Equality.type](name)
    }
  }
}
