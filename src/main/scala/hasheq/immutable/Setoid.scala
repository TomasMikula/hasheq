package hasheq
package immutable

import scala.annotation.tailrec
import scala.language.higherKinds
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

/** Witness that `S[A]` represents an immutable set of elements of type `A`
  * with respect to equivalence relation identified by the type member `Eq`.
  */
trait Setoid[S[_], A] {
  import Setoid._

  /** Identifies the equivalence relation on type `A` that is respected
    * by this setoid.
    */
  type Eq

  def empty: S[A]
  def singleton(a: A)(implicit E: Equiv[A, Eq]): S[A] = add(empty, a)
  def fromIterable(col: Iterable[A])(implicit E: Equiv[A, Eq]): S[A] = fromIterator(col.iterator)
  def fromIterator(it: Iterator[A])(implicit E: Equiv[A, Eq]): S[A] = it.foldLeft(empty)(add(_, _))

  def size(s: S[A]): Int
  def isEmpty(s: S[A]): Boolean = size(s) == 0
  def contains(s: S[A], a: A)(implicit E: Equiv[A, Eq]): Boolean
  def iterator(s: S[A]): Iterator[A]
  def toList(s: S[A]): List[A] = iterator(s).toList
  def foldLeft[B](s: S[A])(b: B)(f: (B, A) => B): B = iterator(s).foldLeft(b)(f)
  def forall(s: S[A], p: A => Boolean): Boolean = iterator(s).forall(p)
  def exists(s: S[A], p: A => Boolean): Boolean = iterator(s).exists(p)
  def subset(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): Boolean =
    size(s1) <= size(s2) && forall(s1, contains(s2, _))

  def add(s: S[A], a: A)(implicit E: Equiv[A, Eq]): S[A]
  def remove(s: S[A], a: A)(implicit E: Equiv[A, Eq]): S[A]
  def addAll(s: S[A], col: Iterable[A])(implicit E: Equiv[A, Eq]): S[A] = col.foldLeft(s)(add(_, _))
  def addAll(s: S[A], it: Iterator[A])(implicit E: Equiv[A, Eq]): S[A] = it.foldLeft(s)(add(_, _))
  def removeAll(s: S[A], col: Iterable[A])(implicit E: Equiv[A, Eq]): S[A] = col.foldLeft(s)(remove(_, _))
  def removeAll(s: S[A], it: Iterator[A])(implicit E: Equiv[A, Eq]): S[A] = it.foldLeft(s)(remove(_, _))
  def retain(s: S[A], p: A => Boolean)(implicit E: Equiv[A, Eq]): S[A] = foldLeft(s)(s)((s, a) => if(!p(a)) remove(s, a) else s)
  def union(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): S[A] =
    if(size(s1) > size(s2)) addAll(s1, iterator(s2))
    else                    addAll(s2, iterator(s1))
  def intersect(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): S[A] =
    if(size(s1) > size(s2)) retain(s2, contains(s1, _))
    else                    retain(s1, contains(s2, _))
  def diff(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): S[A] =
    if(size(s1) > size(s2)) foldLeft(s2)(s1)(remove(_, _))
    else                    retain(s1, !contains(s2, _))
  def symDiff(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): (S[A], S[A]) = (diff(s1, s2), diff(s2, s1))

  def equiv(s1: S[A], s2: S[A])(implicit E: Equiv[A, Eq]): Boolean =
    size(s1) == size(s2) && subset(s1, s2)

  def contentEquivalence(implicit E: Equiv[A, Eq]): Equiv[S[A], ContentEquiv[A, Eq]] = new Equiv[S[A], ContentEquiv[A, Eq]] {
    def equiv(s1: S[A], s2: S[A]): Boolean = Setoid.this.equiv(s1, s2)
  }

  def contentHash(implicit H: HashEq[A, Eq]): HashEq[S[A], ContentEquiv[A, Eq]] = new HashEq[S[A], ContentEquiv[A, Eq]] {
    def hash(s: S[A]): Int = {
      // Cannot take the order of elements into account, only what elements are present.

      // Let's use the size and the 3 highest hash codes

      val top = Array(Int.MinValue, Int.MinValue, Int.MinValue)
      val n = top.length
      val it = iterator(s)
      while (it.hasNext) {
        val h = H.hash(it.next())
        if (h > top(0)) {
          var i = 0
          while (i+1 < n && top(i+1) < h) {
            top(i) = top(i+1)
            i += 1
          }
          top(i) = h
        }
      }

      top.foldLeft(size(s))((acc, h) => 31 * acc + h)
    }
  }
}

object Setoid {
  type Aux[S[_], A, Eqv] = Setoid[S, A] { type Eq = Eqv }

  /** Witness that `S[A, Eqv]` represents an immutable set of elements of type `A`
    * with respect to equivalence relation identified by `Eqv`.
    *
    * This is an auxiliary type alias for setoid implementations that are generic
    * in the type of equivalence used.
    *
    * @tparam S   underlying type of the setoid
    * @tparam A   type of the elements
    * @tparam Eqv identifier of the equivalence
    */
  type Generic[S[_, _], A, Eqv] = Aux[S[?, Eqv], A, Eqv]

  def apply[S[_, _], A, Eq](implicit ev: Setoid.Generic[S, A, Eq]): Setoid.Generic[S, A, Eq] = ev

  sealed trait ContentEquiv[A, Eq]

  implicit def arbitrary[S[_], A, Eq](implicit S: Setoid.Aux[S, A, Eq], A: Arbitrary[A], E: Equiv[A, Eq]): Arbitrary[S[A]] =
    Arbitrary {
      def genSized(n: Int): Gen[S[A]] =
        if (n <= 0) Gen.const(S.empty)
        else for {
          a <- A.arbitrary
          s <- genSized(n-1)
        } yield(S.add(s, a))

      Gen.sized(n => genSized(n))
    }

  trait Laws {

    /*
     * Laws of generalized Boolean algebra that relate the operations
     * union, intersect, empty, diff.
     */

    def unionIdempotence[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.union(s, s), s)

    def intersectIdempotence[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.intersect(s, s), s)

    def unionCommutativity[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.union(s, t), S.union(t, s))

    def intersectCommutativity[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.intersect(s, t), S.intersect(t, s))

    def unionAssociativity[S[_], A, Eq](s: S[A], t: S[A], u: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.union(S.union(s, t), u), S.union(s, S.union(t, u)))

    def intersectAssociativity[S[_], A, Eq](s: S[A], t: S[A], u: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.intersect(S.intersect(s, t), u), S.intersect(s, S.intersect(t, u)))

    def absorption[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.intersect(s, S.union(s, t)), s) &&
      S.equiv(S.union(s, S.intersect(s, t)), s)

    def empty[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.union(s, S.empty), s) &&
      S.equiv(S.intersect(s, S.empty), S.empty)

    def distributivity[S[_], A, Eq](s: S[A], t: S[A], u: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.intersect(s, S.union(t, u)), S.union(S.intersect(s, t), S.intersect(s, u))) &&
      S.equiv(S.union(s, S.intersect(t, u)), S.intersect(S.union(s, t), S.union(s, u)))

    def relativeComplement[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.union(S.diff(s, t), S.intersect(s, t)), s) &&
      S.equiv(S.intersect(S.diff(s, t), S.intersect(s, t)), S.empty)


    /* Remaining operations. */

    def symDiff[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean = {
      val (l, r) = S.symDiff(s, t)
      S.equiv(l, S.diff(s, t)) && S.equiv(r, S.diff(t, s))
    }

    def subset[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.subset(s, t) == S.equiv(S.intersect(s, t), s)

    def size[S[_], A, Eq](s: S[A], t: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.size(s) + S.size(t) == S.size(S.union(s, t)) + S.size(S.intersect(s, t))

    def isEmpty[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq]): Boolean =
      S.isEmpty(s) == (S.size(s) == 0)

    def containsAdded[S[_], A, Eq](s: S[A], a: A)(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.contains(S.add(s, a), a)

    def notContainsRemoved[S[_], A, Eq](s: S[A], a: A)(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      !S.contains(S.remove(s, a), a)

    def foldLeft[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean = {
      val (t, n) = S.foldLeft(s)((S.empty, 0))((acc, a) => (S.add(acc._1, a), acc._2 + 1))
      S.equiv(t, s) && n == S.size(s)
    }

    def forall[S[_], A, Eq](s: S[A], p: A => Boolean)(implicit S: Setoid.Aux[S, A, Eq]): Boolean =
      S.forall(s, p) == S.foldLeft(s)(true)((b, a) => b && p(a))

    def exists[S[_], A, Eq](s: S[A], p: A => Boolean)(implicit S: Setoid.Aux[S, A, Eq]): Boolean =
      S.exists(s, p) == S.foldLeft(s)(false)((b, a) => b || p(a))

    def containsAll[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.forall(s, S.contains(s, _))

    def fromIterable[S[_], A, Eq](col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean = {
      @tailrec def fromList(l: List[A], acc: S[A]): S[A] = l match {
        case a :: as => fromList(as, S.add(acc, a))
        case Nil => acc
      }
      S.equiv(S.fromIterable(col), fromList(col.toList, S.empty))
    }

    def fromIterator[S[_], A, Eq](col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.fromIterator(col.iterator), S.fromIterable(col))

    def toList[S[_], A, Eq](s: S[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.fromIterable(S.toList(s)), s)

    def singleton[S[_], A, Eq](a: A)(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.singleton(a), S.add(S.empty, a))

    def addAll[S[_], A, Eq](s: S[A], col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.addAll(s, col), S.union(s, S.fromIterable(col)))

    def addAllIterator[S[_], A, Eq](s: S[A], col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.addAll(s, col.iterator), S.union(s, S.fromIterable(col)))

    def removeAll[S[_], A, Eq](s: S[A], col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.removeAll(s, col), S.diff(s, S.fromIterable(col)))

    def removeAllIterator[S[_], A, Eq](s: S[A], col: Iterable[A])(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.removeAll(s, col.iterator), S.diff(s, S.fromIterable(col)))

    def retain[S[_], A, Eq](s: S[A], p: A => Boolean)(implicit S: Setoid.Aux[S, A, Eq], E: Equiv[A, Eq]): Boolean =
      S.equiv(S.retain(s, p), S.fromIterable(S.toList(s).filter(p)))
  }

  def properties[S[_], A, Eqv](name: String = "Setoid")(implicit S: Setoid.Aux[S, A, Eqv], E: Equiv[A, Eqv], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
    new Properties(name) {
      val laws = new Laws {}

      property("unionIdempotence")       = forAll(laws.unionIdempotence[S, A, Eqv] _)
      property("intersectIdempotence")   = forAll(laws.intersectIdempotence[S, A, Eqv] _)
      property("unionCommutativity")     = forAll(laws.unionCommutativity[S, A, Eqv] _)
      property("intersectCommutativity") = forAll(laws.intersectCommutativity[S, A, Eqv] _)
      property("unionAssociativity")     = forAll(laws.unionAssociativity[S, A, Eqv] _)
      property("intersectAssociativity") = forAll(laws.intersectAssociativity[S, A, Eqv] _)
      property("absorption")             = forAll(laws.absorption[S, A, Eqv] _)
      property("empty")                  = forAll(laws.empty[S, A, Eqv] _)
      property("distributivity")         = forAll(laws.distributivity[S, A, Eqv] _)
      property("relativeComplement")     = forAll(laws.relativeComplement[S, A, Eqv] _)
      property("symDiff")                = forAll(laws.symDiff[S, A, Eqv] _)
      property("subset")                 = forAll(laws.subset[S, A, Eqv] _)
      property("size")                   = forAll(laws.size[S, A, Eqv] _)
      property("isEmpty")                = forAll(laws.isEmpty[S, A, Eqv] _)
      property("containsAdded")          = forAll(laws.containsAdded[S, A, Eqv] _)
      property("notContainsRemoved")     = forAll(laws.notContainsRemoved[S, A, Eqv] _)
      property("foldLeft")               = forAll(laws.foldLeft[S, A, Eqv] _)
      property("forall")                 = forAll(laws.forall[S, A, Eqv] _)
      property("exists")                 = forAll(laws.exists[S, A, Eqv] _)
      property("containsAll")            = forAll(laws.containsAll[S, A, Eqv] _)
      property("fromIterable")           = forAll(laws.fromIterable[S, A, Eqv] _)
      property("fromIterator")           = forAll(laws.fromIterator[S, A, Eqv] _)
      property("toList")                 = forAll(laws.toList[S, A, Eqv] _)
      property("singleton")              = forAll(laws.singleton[S, A, Eqv] _)
      property("addAll")                 = forAll(laws.addAll[S, A, Eqv] _)
      property("addAllIterator")         = forAll(laws.addAllIterator[S, A, Eqv] _)
      property("removeAll")              = forAll(laws.removeAll[S, A, Eqv] _)
      property("removeAllIterator")      = forAll(laws.removeAllIterator[S, A, Eqv] _)
      property("retain")                 = forAll(laws.retain[S, A, Eqv] _)
    }

  def genProperties[S[_, _], A, Eqv](name: String = "Setoid")(implicit S: Setoid.Generic[S, A, Eqv], E: Equiv[A, Eqv], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
    properties[S[?, Eqv], A, Eqv](name)
}
