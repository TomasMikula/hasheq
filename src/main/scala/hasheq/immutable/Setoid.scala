package hasheq
package immutable

import scala.annotation.tailrec
import scala.language.higherKinds
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

/** Witness that `S[A, Eq]` represents an immutable set of elements of type `A`
  * with respect to equivalence relation `Eq`. */
trait Setoid[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]] {
  import Setoid._

  def empty: S[A, Eq]
  def singleton(a: A)(implicit E: Eq): S[A, Eq] = add(empty, a)
  def fromIterable(col: Iterable[A])(implicit E: Eq): S[A, Eq] = fromIterator(col.iterator)
  def fromIterator(it: Iterator[A])(implicit E: Eq): S[A, Eq] = it.foldLeft(empty)(add(_, _))

  def size(s: S[A, Eq]): Int
  def isEmpty(s: S[A, Eq]): Boolean = size(s) == 0
  def contains(s: S[A, Eq], a: A)(implicit E: Eq): Boolean
  def iterator(s: S[A, Eq]): Iterator[A]
  def toList(s: S[A, Eq]): List[A] = iterator(s).toList
  def foldLeft[B](s: S[A, Eq])(b: B)(f: (B, A) => B): B = iterator(s).foldLeft(b)(f)
  def forall(s: S[A, Eq], p: A => Boolean): Boolean = iterator(s).forall(p)
  def exists(s: S[A, Eq], p: A => Boolean): Boolean = iterator(s).exists(p)
  def subset(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): Boolean =
    size(s1) <= size(s2) && forall(s1, contains(s2, _))

  def add(s: S[A, Eq], a: A)(implicit E: Eq): S[A, Eq]
  def remove(s: S[A, Eq], a: A)(implicit E: Eq): S[A, Eq]
  def addAll(s: S[A, Eq], col: Iterable[A])(implicit E: Eq): S[A, Eq] = col.foldLeft(s)(add(_, _))
  def addAll(s: S[A, Eq], it: Iterator[A])(implicit E: Eq): S[A, Eq] = it.foldLeft(s)(add(_, _))
  def removeAll(s: S[A, Eq], col: Iterable[A])(implicit E: Eq): S[A, Eq] = col.foldLeft(s)(remove(_, _))
  def removeAll(s: S[A, Eq], it: Iterator[A])(implicit E: Eq): S[A, Eq] = it.foldLeft(s)(remove(_, _))
  def retain(s: S[A, Eq], p: A => Boolean)(implicit E: Eq): S[A, Eq] = foldLeft(s)(s)((s, a) => if(!p(a)) remove(s, a) else s)
  def union(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): S[A, Eq] =
    if(size(s1) > size(s2)) addAll(s1, iterator(s2))
    else                    addAll(s2, iterator(s1))
  def intersect(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): S[A, Eq] =
    if(size(s1) > size(s2)) retain(s2, contains(s1, _))
    else                    retain(s1, contains(s2, _))
  def diff(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): S[A, Eq] =
    if(size(s1) > size(s2)) foldLeft(s2)(s1)(remove(_, _))
    else                    retain(s1, !contains(s2, _))
  def symDiff(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): (S[A, Eq], S[A, Eq]) = (diff(s1, s2), diff(s2, s1))

  def equiv(s1: S[A, Eq], s2: S[A, Eq])(implicit E: Eq): Boolean =
    size(s1) == size(s2) && subset(s1, s2)

  def contentEquivalence(implicit E: Eq): SetEquiv[S, A, Eq] = new SetEquiv[S, A, Eq] {
    def equiv(s1: S[A, Eq], s2: S[A, Eq]): Boolean = Setoid.this.equiv(s1, s2)
  }

  def contentHash(implicit H: Hash[A, Eq]): Hash[S[A, Eq], SetEquiv[S, A, Eq]] = new Hash[S[A, Eq], SetEquiv[S, A, Eq]] {
    def hash(s: S[A, Eq]): Int = {
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
  def apply[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](implicit ev: Setoid[S, A, Eq]): Setoid[S, A, Eq] = ev

  trait SetEquiv[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]] extends Equiv[S[A, Eq]]

  implicit def arbitrary[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](implicit S: Setoid[S, A, Eq], A: Arbitrary[A], E: Eq): Arbitrary[S[A, Eq]] =
    Arbitrary {
      def genSized(n: Int): Gen[S[A, Eq]] =
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

    def unionIdempotence[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.union(s, s), s)

    def intersectIdempotence[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.intersect(s, s), s)

    def unionCommutativity[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.union(s, t), S.union(t, s))

    def intersectCommutativity[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.intersect(s, t), S.intersect(t, s))

    def unionAssociativity[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq], u: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.union(S.union(s, t), u), S.union(s, S.union(t, u)))

    def intersectAssociativity[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq], u: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.intersect(S.intersect(s, t), u), S.intersect(s, S.intersect(t, u)))

    def absorption[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.intersect(s, S.union(s, t)), s) &&
      S.equiv(S.union(s, S.intersect(s, t)), s)

    def empty[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.union(s, S.empty), s) &&
      S.equiv(S.intersect(s, S.empty), S.empty)

    def distributivity[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq], u: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.intersect(s, S.union(t, u)), S.union(S.intersect(s, t), S.intersect(s, u))) &&
      S.equiv(S.union(s, S.intersect(t, u)), S.intersect(S.union(s, t), S.union(s, u)))

    def relativeComplement[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.union(S.diff(s, t), S.intersect(s, t)), s) &&
      S.equiv(S.intersect(S.diff(s, t), S.intersect(s, t)), S.empty)


    /* Remaining operations. */

    def symDiff[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean = {
      val (l, r) = S.symDiff(s, t)
      S.equiv(l, S.diff(s, t)) && S.equiv(r, S.diff(t, s))
    }

    def subset[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.subset(s, t) == S.equiv(S.intersect(s, t), s)

    def size[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], t: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.size(s) + S.size(t) == S.size(S.union(s, t)) + S.size(S.intersect(s, t))

    def isEmpty[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq]): Boolean =
      S.isEmpty(s) == (S.size(s) == 0)

    def containsAdded[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], a: A)(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.contains(S.add(s, a), a)

    def notContainsRemoved[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], a: A)(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      !S.contains(S.remove(s, a), a)

    def foldLeft[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean = {
      val (t, n) = S.foldLeft(s)((S.empty, 0))((acc, a) => (S.add(acc._1, a), acc._2 + 1))
      S.equiv(t, s) && n == S.size(s)
    }

    def forall[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], p: A => Boolean)(implicit S: Setoid[S, A, Eq]): Boolean =
      S.forall(s, p) == S.foldLeft(s)(true)((b, a) => b && p(a))

    def exists[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], p: A => Boolean)(implicit S: Setoid[S, A, Eq]): Boolean =
      S.exists(s, p) == S.foldLeft(s)(false)((b, a) => b || p(a))

    def containsAll[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], A: Equiv[A], E: Eq): Boolean =
      S.forall(s, S.contains(s, _))

    def fromIterable[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean = {
      @tailrec def fromList(l: List[A], acc: S[A, Eq]): S[A, Eq] = l match {
        case a :: as => fromList(as, S.add(acc, a))
        case Nil => acc
      }
      S.equiv(S.fromIterable(col), fromList(col.toList, S.empty))
    }

    def fromIterator[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.fromIterator(col.iterator), S.fromIterable(col))

    def toList[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.fromIterable(S.toList(s)), s)

    def singleton[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](a: A)(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.singleton(a), S.add(S.empty, a))

    def addAll[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.addAll(s, col), S.union(s, S.fromIterable(col)))

    def addAllIterator[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.addAll(s, col.iterator), S.union(s, S.fromIterable(col)))

    def removeAll[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.removeAll(s, col), S.diff(s, S.fromIterable(col)))

    def removeAllIterator[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], col: Iterable[A])(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.removeAll(s, col.iterator), S.diff(s, S.fromIterable(col)))

    def retain[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](s: S[A, Eq], p: A => Boolean)(implicit S: Setoid[S, A, Eq], E: Eq): Boolean =
      S.equiv(S.retain(s, p), S.fromIterable(S.toList(s).filter(p)))
  }

  def properties[S[A0, _ <: Equiv[A0]], A, Eq <: Equiv[A]](name: String = "Setoid")(implicit S: Setoid[S, A, Eq], E: Eq, A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
    new Properties(name) {
      val laws = new Laws {}

      property("unionIdempotence")       = forAll(laws.unionIdempotence[S, A, Eq] _)
      property("intersectIdempotence")   = forAll(laws.intersectIdempotence[S, A, Eq] _)
      property("unionCommutativity")     = forAll(laws.unionCommutativity[S, A, Eq] _)
      property("intersectCommutativity") = forAll(laws.intersectCommutativity[S, A, Eq] _)
      property("unionAssociativity")     = forAll(laws.unionAssociativity[S, A, Eq] _)
      property("intersectAssociativity") = forAll(laws.intersectAssociativity[S, A, Eq] _)
      property("absorption")             = forAll(laws.absorption[S, A, Eq] _)
      property("empty")                  = forAll(laws.empty[S, A, Eq] _)
      property("distributivity")         = forAll(laws.distributivity[S, A, Eq] _)
      property("relativeComplement")     = forAll(laws.relativeComplement[S, A, Eq] _)
      property("symDiff")                = forAll(laws.symDiff[S, A, Eq] _)
      property("subset")                 = forAll(laws.subset[S, A, Eq] _)
      property("size")                   = forAll(laws.size[S, A, Eq] _)
      property("isEmpty")                = forAll(laws.isEmpty[S, A, Eq] _)
      property("containsAdded")          = forAll(laws.containsAdded[S, A, Eq] _)
      property("notContainsRemoved")     = forAll(laws.notContainsRemoved[S, A, Eq] _)
      property("foldLeft")               = forAll(laws.foldLeft[S, A, Eq] _)
      property("forall")                 = forAll(laws.forall[S, A, Eq] _)
      property("exists")                 = forAll(laws.exists[S, A, Eq] _)
      property("containsAll")            = forAll(laws.containsAll[S, A, Eq] _)
      property("fromIterable")           = forAll(laws.fromIterable[S, A, Eq] _)
      property("fromIterator")           = forAll(laws.fromIterator[S, A, Eq] _)
      property("toList")                 = forAll(laws.toList[S, A, Eq] _)
      property("singleton")              = forAll(laws.singleton[S, A, Eq] _)
      property("addAll")                 = forAll(laws.addAll[S, A, Eq] _)
      property("addAllIterator")         = forAll(laws.addAllIterator[S, A, Eq] _)
      property("removeAll")              = forAll(laws.removeAll[S, A, Eq] _)
      property("removeAllIterator")      = forAll(laws.removeAllIterator[S, A, Eq] _)
      property("retain")                 = forAll(laws.retain[S, A, Eq] _)
    }
}
