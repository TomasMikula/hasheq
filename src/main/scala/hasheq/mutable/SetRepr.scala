package hasheq
package mutable

import scala.language.higherKinds

/** Witness that `S[A]` represents a mutable set of elements of type `A`. */
trait SetRepr[S[_], A] {

  def empty: S[A]
  def fromIterable(col: Iterable[A]): S[A] = fromIterator(col.iterator)
  def fromIterator(it: Iterator[A]): S[A] = {
    val res = empty
    addAll(res, it)
    res
  }

  def size(s: S[A]): Int
  def isEmpty(s: S[A]): Boolean = size(s) == 0
  def contains(s: S[A], a: A): Boolean = find(s, a).isDefined
  def find(s: S[A], a: A): Option[A]
  def iterator(s: S[A]): Iterator[A]
  def toList(s: S[A]): List[A] = iterator(s).toList

  def add(s: S[A], a: A): Boolean
  def remove(s: S[A], a: A): Boolean
  def addAll(s: S[A], col: Iterable[A]): Unit = col.foreach(add(s, _))
  def addAll(s: S[A], it: Iterator[A]): Unit = it.foreach(add(s, _))
  def removeAll(s: S[A], col: Iterable[A]): Unit = col.foreach(remove(s, _))
  def retain(s: S[A], p: A => Boolean): Unit = toList(s).foreach(a => if (!p(a)) remove(s, a))
}

object SetRepr {
  def apply[S[_], A](implicit ev: SetRepr[S, A]): SetRepr[S, A] = ev
}
