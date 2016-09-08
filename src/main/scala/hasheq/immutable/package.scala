package hasheq

import org.scalacheck.{Arbitrary, Properties}

import scala.language.higherKinds

package object immutable {

  /** Witness that `S[A]` represents an immutable set of elements of type `A`. */
  type SetRepr[S[_], A] = Setoid.Aux[S, A, Equality.type]
  object SetRepr {
    def properties[S[_], A](name: String = "SetRepr")(implicit S: SetRepr[S, A], EA: Equal[A], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
      Setoid.properties[S, A, Equality.type](name)
  }

  type HashSet[A] = HashSetoid[A, Equality.type]
  object HashSet {
    def empty[A]: HashSet[A] = HashSetoid.empty[A, Equality.type]
    def apply[A](elems: A*)(implicit A: HashEq[A, Equality.type], E: Equal[A]): HashSet[A] = HashSetoid(elems:_*)

    def setoidInstance[A](implicit A: Hash[A]): SetRepr[HashSet, A] = HashSetoid.setoidInstance[A, Equality.type]
  }
}
