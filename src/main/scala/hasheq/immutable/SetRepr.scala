package hasheq
package immutable

import scala.annotation.tailrec
import scala.language.higherKinds

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

/** Witness that `S[A]` represents an inmutable set of elements of type `A`. */
trait SetRepr[S[_], A] {

  def empty: S[A]
  def singleton(a: A): S[A] = add(empty, a)
  def fromIterable(col: Iterable[A]): S[A] = fromIterator(col.iterator)
  def fromIterator(it: Iterator[A]): S[A] = it.foldLeft(empty)(add(_, _))

  def size(s: S[A]): Int
  def isEmpty(s: S[A]): Boolean = size(s) == 0
  def contains(s: S[A], a: A): Boolean
  def iterator(s: S[A]): Iterator[A]
  def toList(s: S[A]): List[A] = iterator(s).toList
  def foldLeft[B](s: S[A])(b: B)(f: (B, A) => B): B = iterator(s).foldLeft(b)(f)
  def forall(s: S[A], p: A => Boolean): Boolean = iterator(s).forall(p)
  def exists(s: S[A], p: A => Boolean): Boolean = iterator(s).exists(p)
  def subset(s1: S[A], s2: S[A]): Boolean =
    size(s1) <= size(s2) && forall(s1, contains(s2, _))

  def add(s: S[A], a: A): S[A]
  def remove(s: S[A], a: A): S[A]
  def addAll(s: S[A], col: Iterable[A]): S[A] = col.foldLeft(s)(add(_, _))
  def addAll(s: S[A], it: Iterator[A]): S[A] = it.foldLeft(s)(add(_, _))
  def removeAll(s: S[A], col: Iterable[A]): S[A] = col.foldLeft(s)(remove(_, _))
  def removeAll(s: S[A], it: Iterator[A]): S[A] = it.foldLeft(s)(remove(_, _))
  def retain(s: S[A], p: A => Boolean): S[A] = foldLeft(s)(s)((s, a) => if(!p(a)) remove(s, a) else s)
  def union(s1: S[A], s2: S[A]): S[A] =
    if(size(s1) > size(s2)) addAll(s1, iterator(s2))
    else                    addAll(s2, iterator(s1))
  def intersect(s1: S[A], s2: S[A]): S[A] =
    if(size(s1) > size(s2)) retain(s2, contains(s1, _))
    else                    retain(s1, contains(s2, _))
  def diff(s1: S[A], s2: S[A]): S[A] =
    if(size(s1) > size(s2)) foldLeft(s2)(s1)(remove(_, _))
    else                    retain(s1, !contains(s2, _))
  def symDiff(s1: S[A], s2: S[A]): (S[A], S[A]) = (diff(s1, s2), diff(s2, s1))

  def equal(s1: S[A], s2: S[A]): Boolean =
    size(s1) == size(s2) && subset(s1, s2)

  def contentEquality: Eq[S[A]] = new Eq[S[A]] {
    def equal(s1: S[A], s2: S[A]): Boolean = SetRepr.this.equal(s1, s2)
  }
}

object SetRepr {
  def apply[S[_], A](implicit ev: SetRepr[S, A]): SetRepr[S, A] = ev

  implicit def arbitrary[S[_], A](implicit S: SetRepr[S, A], A: Arbitrary[A]): Arbitrary[S[A]] =
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

    def unionIdempotence[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.union(s, s), s)

    def intersectIdempotence[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.intersect(s, s), s)

    def unionCommutativity[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.union(s, t), S.union(t, s))

    def intersectCommutativity[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.intersect(s, t), S.intersect(t, s))

    def unionAssociativity[S[_], A](s: S[A], t: S[A], u: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.union(S.union(s, t), u), S.union(s, S.union(t, u)))

    def intersectAssociativity[S[_], A](s: S[A], t: S[A], u: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.intersect(S.intersect(s, t), u), S.intersect(s, S.intersect(t, u)))

    def absorption[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.intersect(s, S.union(s, t)), s) &&
      S.equal(S.union(s, S.intersect(s, t)), s)

    def empty[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.union(s, S.empty), s) &&
      S.equal(S.intersect(s, S.empty), S.empty)

    def distributivity[S[_], A](s: S[A], t: S[A], u: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.intersect(s, S.union(t, u)), S.union(S.intersect(s, t), S.intersect(s, u))) &&
      S.equal(S.union(s, S.intersect(t, u)), S.intersect(S.union(s, t), S.union(s, u)))

    def relativeComplement[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.union(S.diff(s, t), S.intersect(s, t)), s) &&
      S.equal(S.intersect(S.diff(s, t), S.intersect(s, t)), S.empty)


    /* Remaining operations. */

    def symDiff[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean = {
      val (l, r) = S.symDiff(s, t)
      S.equal(l, S.diff(s, t)) && S.equal(r, S.diff(t, s))
    }

    def subset[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.subset(s, t) == S.equal(S.intersect(s, t), s)

    def size[S[_], A](s: S[A], t: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.size(s) + S.size(t) == S.size(S.union(s, t)) + S.size(S.intersect(s, t))

    def isEmpty[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.isEmpty(s) == (S.size(s) == 0)

    def containsAdded[S[_], A](s: S[A], a: A)(implicit S: SetRepr[S, A]): Boolean =
      S.contains(S.add(s, a), a)

    def notContainsRemoved[S[_], A](s: S[A], a: A)(implicit S: SetRepr[S, A]): Boolean =
      !S.contains(S.remove(s, a), a)

    def foldLeft[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean = {
      val (t, n) = S.foldLeft(s)((S.empty, 0))((acc, a) => (S.add(acc._1, a), acc._2 + 1))
      S.equal(t, s) && n == S.size(s)
    }

    def forall[S[_], A](s: S[A], p: A => Boolean)(implicit S: SetRepr[S, A]): Boolean =
      S.forall(s, p) == S.foldLeft(s)(true)((b, a) => b && p(a))

    def exists[S[_], A](s: S[A], p: A => Boolean)(implicit S: SetRepr[S, A]): Boolean =
      S.exists(s, p) == S.foldLeft(s)(false)((b, a) => b || p(a))

    def containsAll[S[_], A](s: S[A])(implicit S: SetRepr[S, A], A: Eq[A]): Boolean =
      S.forall(s, S.contains(s, _))

    def fromIterable[S[_], A](col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean = {
      @tailrec def fromList(l: List[A], acc: S[A]): S[A] = l match {
        case a :: as => fromList(as, S.add(acc, a))
        case Nil => acc
      }
      S.equal(S.fromIterable(col), fromList(col.toList, S.empty))
    }

    def fromIterator[S[_], A](col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.fromIterator(col.iterator), S.fromIterable(col))

    def toList[S[_], A](s: S[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.fromIterable(S.toList(s)), s)

    def singleton[S[_], A](a: A)(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.singleton(a), S.add(S.empty, a))

    def addAll[S[_], A](s: S[A], col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.addAll(s, col), S.union(s, S.fromIterable(col)))

    def addAllIterator[S[_], A](s: S[A], col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.addAll(s, col.iterator), S.union(s, S.fromIterable(col)))

    def removeAll[S[_], A](s: S[A], col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.removeAll(s, col), S.diff(s, S.fromIterable(col)))

    def removeAllIterator[S[_], A](s: S[A], col: Iterable[A])(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.removeAll(s, col.iterator), S.diff(s, S.fromIterable(col)))

    def retain[S[_], A](s: S[A], p: A => Boolean)(implicit S: SetRepr[S, A]): Boolean =
      S.equal(S.retain(s, p), S.fromIterable(S.toList(s).filter(p)))
  }

  def properties[S[_], A](name: String = "SetRepr")(implicit S: SetRepr[S, A], EA: Eq[A], A: Arbitrary[A], PA: Arbitrary[A => Boolean]): Properties =
    new Properties(name) {
      val laws = new Laws {}

      property("unionIdempotence")       = forAll(laws.unionIdempotence[S, A] _)
      property("intersectIdempotence")   = forAll(laws.intersectIdempotence[S, A] _)
      property("unionCommutativity")     = forAll(laws.unionCommutativity[S, A]_)
      property("intersectCommutativity") = forAll(laws.intersectCommutativity[S, A] _)
      property("unionAssociativity")     = forAll(laws.unionAssociativity[S, A] _)
      property("intersectAssociativity") = forAll(laws.intersectAssociativity[S, A] _)
      property("absorption")             = forAll(laws.absorption[S, A] _)
      property("empty")                  = forAll(laws.empty[S, A] _)
      property("distributivity")         = forAll(laws.distributivity[S, A] _)
      property("relativeComplement")     = forAll(laws.relativeComplement[S, A] _)
      property("symDiff")                = forAll(laws.symDiff[S, A] _)
      property("subset")                 = forAll(laws.subset[S, A] _)
      property("size")                   = forAll(laws.size[S, A] _)
      property("isEmpty")                = forAll(laws.isEmpty[S, A] _)
      property("containsAdded")          = forAll(laws.containsAdded[S, A] _)
      property("notContainsRemoved")     = forAll(laws.notContainsRemoved[S, A] _)
      property("foldLeft")               = forAll(laws.foldLeft[S, A] _)
      property("forall")                 = forAll(laws.forall[S, A] _)
      property("exists")                 = forAll(laws.exists[S, A] _)
      property("containsAll")            = forAll(laws.containsAll[S, A] _)
      property("fromIterable")           = forAll(laws.fromIterable[S, A] _)
      property("fromIterator")           = forAll(laws.fromIterator[S, A] _)
      property("toList")                 = forAll(laws.toList[S, A] _)
      property("singleton")              = forAll(laws.singleton[S, A] _)
      property("addAll")                 = forAll(laws.addAll[S, A] _)
      property("addAllIterator")         = forAll(laws.addAllIterator[S, A] _)
      property("removeAll")              = forAll(laws.removeAll[S, A] _)
      property("removeAllIterator")      = forAll(laws.removeAllIterator[S, A] _)
      property("retain")                 = forAll(laws.retain[S, A] _)
    }
}
